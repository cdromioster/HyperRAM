library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
--use work.debugtools.all;
use work.cputypes.all;

entity hyperram_mega65 is
  port (
    pixelclock        : in  std_logic; -- for slow devices bus interface is
    clock163          : in  std_logic; -- Used for fast clock for HyperRAM
    clock325          : in  std_logic; -- Used for fast clock for HyperRAM SERDES units

    request_counter   : out std_logic := '0';
    read_request      : in  std_logic;
    write_request     : in  std_logic;
    address           : in  unsigned(26 downto 0);
    wdata             : in  unsigned(7 downto 0);
    wdata_hi          : in  unsigned(7 downto 0);
    wen_hi            : in  std_logic;
    wen_lo            : in  std_logic;
    rdata_hi          : out unsigned(7 downto 0);
    rdata_16en        : in  std_logic;         -- set this high to be able to read 16-bit values
    rdata             : out unsigned(7 downto 0);
    data_ready_strobe : out std_logic := '0';
    busy              : out std_logic := '0';

    hr_d_in           : in  unsigned(7 downto 0); -- Data/Address
    hr_rwds_in        : in  std_logic; -- RW Data strobe
    hr_d_out          : out unsigned(7 downto 0); -- Data/Address
    hr_rwds_out       : out std_logic; -- RW Data strobe
    hr_d_oe           : out std_logic := '0';
    hr_rwds_oe        : out std_logic := '0';
    hr_reset          : out std_logic := '1'; -- Active low RESET line to HyperRAM
    hr_clk_p          : out std_logic := '1';
    hr_cs0            : out std_logic := '1'
  );
end entity hyperram_mega65;

architecture synthesis of hyperram_mega65 is

  type state_t is (
    StartupDelay,
    ReadAbort,
    Idle,
    ReadSetup,
    WriteSetup,
    HyperRAMOutputCommandSlow,
    StartBackgroundWrite,
    HyperRAMDoWriteSlow,
    HyperRAMFinishWriting,
    HyperRAMReadWaitSlow
  );

  -- How many clock ticks need to expire between transactions to satisfy T_RWR
  -- of hyperrram for the T_RWR 40ns delay.
  -- We can also subtract one cycle for the time it takes to pull CS low, and then
  -- two more for the clocks before the critical moment, and one more for time
  -- covered by various latencies in the system (including clock 1/2 cycle delay).
  -- This effectively gets us down to 45ns. Taking another cycle would leave us
  -- at only 38.7ns, which is a bit too short.
  -- This gives us an effective 8-byte write latency of ~132ns = ~7.5MHz.
  -- For read it is ~143ns = 6.99MHz, which might just be a whisker too slow
  -- for MiniMig.  By reading only 4 bytes instead of 8, this would allow getting
  -- back down to ~120 -- 132ns, which should be enough.
  -- Actually, all of that is a bit moot, since it seems that we just have to apply
  -- some trial and error to get it right. 1 seems right with the current settings.
  constant C_RWR_DELAY : unsigned(7 downto 0) := to_unsigned(1,8);
  -- 4 is correct for the part we have in the MEGA65, after we have set the
  -- config register to minimise latency.
  constant C_WRITE_LATENCY : unsigned(7 downto 0) := to_unsigned(5,8);
  -- And the matching extra latency is 5
  constant C_EXTRA_WRITE_LATENCY : unsigned(7 downto 0) := to_unsigned(7,8);
  constant C_READ_PHASE_SHIFT : std_logic := '0';
  constant C_WRITE_PHASE_SHIFT : std_logic := '1';
  constant C_WRITE_CONTINUES_MAX : integer range 0 to 255 := 16;
  constant C_READ_TIME_ADJUST : integer range 0 to 255 := 0;

  signal x4_hr_clk_phaseshift_current : std_logic := '1';
  signal x4_hr_clk_fast_current : std_logic := '1';
  signal x4_hr_clock_phase : unsigned(2 downto 0) := "000";

  signal state : state_t := StartupDelay;
  signal x2_rwr_counter : unsigned(7 downto 0) := (others => '0');
  signal x2_rwr_waiting : std_logic := '0';
  signal x2_busy_internal : std_logic := '1';
  signal x2_hr_command : unsigned(47 downto 0);

  -- Initial transaction is config register write
  signal x2_config_reg_write : std_logic := '1';
  signal x2_ram_reading_held : std_logic := '0';

  -- We want to set config register 0 to $ffe6, to enable variable latency
  -- and 3 cycles instead of 6 for latency. This speeds up writing almost 2x.
  -- But at 80MHz instead of 40MHz bus, we have to increase the latency from
  -- 3 to 4 cycles to satisfy the 40ns minimum time requirement.
  -- This also sets the drive strength to the maximum, to get cleaner faster
  -- clock transitions. This fixes checkerboard read errors at 80MHz.
  signal x2_conf_buf0 : unsigned(7 downto 0) := x"ff";
  signal x2_conf_buf1 : unsigned(7 downto 0) := x"f6";
  signal x2_countdown : integer range 0 to 63 := 0;
  signal x2_countdown_is_zero : std_logic := '1';
  signal x2_extra_latency : std_logic := '0';
  signal x2_countdown_timeout : std_logic := '0';
  signal x2_pause_phase : std_logic := '0';
  signal x2_data_ready_strobe_hold : std_logic := '0';
  signal x2_request_accepted : std_logic := '0';
  signal x2_last_request_toggle : std_logic := '0';
  signal x2_byte_phase : unsigned(5 downto 0) := to_unsigned(0,6);
  signal x2_write_byte_phase : std_logic := '0';
  signal x2_cycle_count : integer := 0;
  signal x2_write_collect0_toolate : std_logic := '0'; -- Set when its too late to
  signal x2_write_collect0_flushed : std_logic := '1';
  signal x2_is_expected_to_respond : boolean := false;
  signal x2_hr_rwds_high_seen : std_logic := '0';
  signal x2_background_write : std_logic := '0';
  signal x2_background_write_valids : std_logic_vector(0 to 7) := x"00";
  signal x2_background_write_data : cache_row_t := (others => (others => '0'));
  signal x2_background_write_count : integer range 0 to 7 := 0;
  signal x2_background_write_next_address : unsigned(26 downto 3) := (others => '0');
  signal x2_background_write_next_address_matches_collect0 : std_logic := '0';
  signal x2_background_chained_write : std_logic := '0';
  signal x2_background_write_fetch : std_logic := '0';
  signal x2_write_continues : integer range 0 to 255 := 0;

  -- Delay sending of the initial configuration write command
  -- to give the HyperRAM chip time to start up
  -- Datasheet says 150usec is required, we do that, plus a bit.
  signal x2_start_delay_counter : integer
--    := 150*(1000/162)+20
    := 150*200+20
    -- plus a correction factor to get initial config register write correctly
    -- aligned with the clock
    +2;
  signal x2_start_delay_expired : std_logic := '0';

  -- phaseshift has to also start at 1 for the above to work.
  signal x2_hr_clk_phaseshift : std_logic := '1';
  signal x2_hr_clk_fast : std_logic := '1';
  signal x2_hr_clock_phase165 : unsigned(1 downto 0) := "00";
  signal x2_seven_plus_read_time_adjust : unsigned(5 downto 0) := "000000";
  signal x2_hyperram_access_address_read_time_adjusted : unsigned(5 downto 0) := "000000";
  signal x2_hyperram_access_address : unsigned(26 downto 0) := to_unsigned(0,27);
  signal x2_read_request_held : std_logic := '0';
  signal x2_write_request_held : std_logic := '0';
  signal x2_read_request_delatch : std_logic := '0';

  -- Collect writes together to hide write latency
  signal x1_ram_address : unsigned(26 downto 0) :=
    "010000000000001000000000000"; -- = bottom 27 bits of x"A001000";
  signal x1_request_toggle : std_logic := '0';
  signal x1_write_collect0_dispatchable : std_logic := '0';
  signal x1_write_collect0_address : unsigned(26 downto 3) := (others => '0');
  signal x1_write_collect0_valids : std_logic_vector(0 to 7) := (others => '0');
  signal x1_write_collect0_data : cache_row_t := ( others => x"00" );
  signal x1_ram_normalfetch : boolean := false;
  signal x1_request_counter_int : std_logic := '1';
  signal x1_write_blocked : std_logic := '0';
  signal x1_read_request_latch : std_logic := '0';
  signal x1_write_request_latch : std_logic := '0';

  -- If we get too many writes in short succession, we may need to queue up to
  -- two of the writes, while waiting for slow_devices to notice
  signal x1_queued_write : std_logic := '0';
  signal x1_queued_wen_lo : std_logic := '0';
  signal x1_queued_wen_hi : std_logic := '0';
  signal x1_queued_wdata : unsigned(7 downto 0) := x"00";
  signal x1_queued_wdata_hi : unsigned(7 downto 0) := x"00";
  signal x1_queued_waddr : unsigned(26 downto 0) := to_unsigned(0,27);
  signal x1_queued2_write : std_logic := '0';
  signal x1_queued2_wen_lo : std_logic := '0';
  signal x1_queued2_wen_hi : std_logic := '0';
  signal x1_queued2_wdata : unsigned(7 downto 0) := x"00";
  signal x1_queued2_wdata_hi : unsigned(7 downto 0) := x"00";
  signal x1_queued2_waddr : unsigned(26 downto 0) := to_unsigned(0,27);

begin

  process (pixelclock) is
  begin
    if rising_edge(pixelclock) then

      if read_request='1' then
        x1_read_request_latch <= '1';
      end if;
      if write_request='1' then
        x1_write_request_latch <= '1';
      end if;
      if x2_read_request_delatch = '1' then
        x1_read_request_latch <= '0';
      end if;

      -- With no cache, we have to IMMEDIATELY assert busy when we see a
      -- request to avoid a race-condition with slow_devices
      busy <= x2_busy_internal or x1_write_blocked or x1_queued_write or x1_queued2_write
              or read_request or write_request or x1_read_request_latch or x1_write_request_latch
              or (not x2_start_delay_expired);

      -- Clear write block as soon as either write buffer clears
      if (x1_write_collect0_dispatchable='0' and x2_write_collect0_toolate='0' and x2_write_collect0_flushed='0')
      then
        x1_write_blocked <= x1_queued_write or x1_queued2_write;
      else
        x1_write_blocked <= '1';
        busy <= '1';
      end if;

      if read_request = '1' or write_request = '1' or x1_read_request_latch='1' or x1_write_request_latch='1' then
        x1_request_counter_int <= not x1_request_counter_int;
        request_counter <= x1_request_counter_int;
      end if;

      -- Clear write buffers once they have been flushed.
      -- We have to wipe the address and valids, so that they don't get stuck being
      -- used as stale sources for cache reading.
      if x1_write_collect0_dispatchable = '1' and x2_write_collect0_toolate = '1' and x2_write_collect0_flushed = '1' then
        report "WRITE: Clearing collect0";
        x1_write_collect0_address <= (others => '1');
        x1_write_collect0_dispatchable <= '0';
      end if;

      if x1_write_collect0_dispatchable = '0' and x2_write_collect0_toolate = '0' and x2_write_collect0_flushed = '0' then
        if x1_queued_write='1' then
          report "DISPATCH: Dequeuing queued write to $" & to_hstring(x1_queued_waddr);

          -- Push it out as a normal batched write, that can collect others if they
          -- come soon enough.

          x1_write_collect0_valids <= (others => '0');
          if x1_queued_wen_lo='1' then
            x1_write_collect0_valids(to_integer(x1_queued_waddr(2 downto 0))) <= '1';
            x1_write_collect0_data(to_integer(x1_queued_waddr(2 downto 0))) <= x1_queued_wdata;
          end if;
          if x1_queued_wen_hi='1' then
            x1_write_collect0_valids(to_integer(x1_queued_waddr(2 downto 0))+1) <= '1';
            x1_write_collect0_data(to_integer(x1_queued_waddr(2 downto 0))+1) <= x1_queued_wdata_hi;
          end if;
          x1_write_collect0_address <= x1_queued_waddr(26 downto 3);
          x1_write_collect0_dispatchable <= '1';

          x1_queued_write <= '0';
        elsif x1_queued2_write='1' then
          report "DISPATCH: Dequeuing queued write to $" & to_hstring(x1_queued2_waddr);

          -- Push it out as a normal batched write, that can collect others if they
          -- come soon enough.

          x1_write_collect0_valids <= (others => '0');
          if x1_queued2_wen_lo='1' then
            x1_write_collect0_valids(to_integer(x1_queued2_waddr(2 downto 0))) <= '1';
            x1_write_collect0_data(to_integer(x1_queued2_waddr(2 downto 0))) <= x1_queued2_wdata;
          end if;
          if x1_queued2_wen_hi='1' then
            x1_write_collect0_valids(to_integer(x1_queued2_waddr(2 downto 0))+1) <= '1';
            x1_write_collect0_data(to_integer(x1_queued2_waddr(2 downto 0))+1) <= x1_queued2_wdata_hi;
          end if;
          x1_write_collect0_address <= x1_queued2_waddr(26 downto 3);
          x1_write_collect0_dispatchable <= '1';

          x1_queued2_write <= '0';
        end if;
      end if;

      -- Ignore read requests to the current block read, as they get
      -- short-circuited in the inner state machine to save time.
      if (read_request or x1_read_request_latch)='1' and x2_busy_internal='0' then
        report "Making read request for $" & to_hstring(address);
        -- Begin read request

        x1_read_request_latch <= '0';

        if x2_request_accepted = x1_request_toggle then
          -- Normal RAM read.
          report "x1_request_toggle flipped";
          x1_ram_address <= address;
          x1_ram_normalfetch <= true;
          x1_request_toggle <= not x1_request_toggle;
        end if;
      elsif x1_queued_write='1' and x1_write_collect0_dispatchable='0' and x2_write_collect0_flushed='0'
        and x2_write_collect0_toolate='0' then

        report "DISPATCH: Executing queued write to $" & to_hstring(x1_queued_waddr);

        -- Push it out as a normal batched write, that can collect others if they
        -- come soon enough.

        x1_write_collect0_valids <= (others => '0');
        if x1_queued_wen_lo='1' then
          x1_write_collect0_valids(to_integer(x1_queued_waddr(2 downto 0))) <= '1';
          x1_write_collect0_data(to_integer(x1_queued_waddr(2 downto 0))) <= x1_queued_wdata;
        end if;
        if x1_queued_wen_hi='1' then
          x1_write_collect0_valids(to_integer(x1_queued_waddr(2 downto 0))+1) <= '1';
          x1_write_collect0_data(to_integer(x1_queued_waddr(2 downto 0))+1) <= x1_queued_wdata_hi;
        end if;
        x1_write_collect0_address <= x1_queued_waddr(26 downto 3);
        x1_write_collect0_dispatchable <= '1';

        x1_queued_write <= '0';

      elsif (write_request or x1_write_request_latch)='1' and x2_busy_internal='0' then
        report "Making write request: addr $" & to_hstring(address) & " <= " & to_hstring(wdata);
        x1_write_request_latch <= '0';

        -- Can we add the write to an existing collected write?
        if x2_write_collect0_toolate = '0' and x1_write_collect0_address = address(26 downto 3)
          and x1_write_collect0_dispatchable = '1' then
          if wen_lo='1' then
            x1_write_collect0_valids(to_integer(address(2 downto 0))) <= '1';
            x1_write_collect0_data(to_integer(address(2 downto 0))) <= wdata;
          end if;
          if wen_hi='1' then
            x1_write_collect0_valids(to_integer(address(2 downto 0))+1) <= '1';
            x1_write_collect0_data(to_integer(address(2 downto 0))+1) <= wdata_hi;
          end if;
        elsif x1_write_collect0_dispatchable = '0' and x2_write_collect0_toolate='0' then
          x1_write_collect0_valids <= (others => '0');
          if wen_lo='1' then
            x1_write_collect0_valids(to_integer(address(2 downto 0))) <= '1';
            x1_write_collect0_data(to_integer(address(2 downto 0))) <= wdata;
          end if;
          if wen_hi='1' then
            x1_write_collect0_valids(to_integer(address(2 downto 0))+1) <= '1';
            x1_write_collect0_data(to_integer(address(2 downto 0))+1) <= wdata_hi;
          end if;

          x1_write_collect0_address <= address(26 downto 3);
          x1_write_collect0_dispatchable <= '1';
          -- Block further writes if we already have one busy write buffer
          x1_write_blocked <= '1';
        else
          -- No write collection point that we can use, so just block until
          -- one becomes available
          report "DISPATCH: Write blocked due to busy write buffers: " &
            " addr $" & to_hstring(address) & " <= " & to_hstring(wdata);
          if x1_queued_write='1' then
            -- Bother. We already had a queued write.
            -- So remember that one, too
            report "Stashing in queued2";
            x1_queued2_waddr <= address;
            x1_queued2_wdata <= wdata;
            x1_queued2_wdata_hi <= wdata_hi;
            x1_queued2_wen_lo <= wen_lo;
            x1_queued2_wen_hi <= wen_hi;
            x1_queued2_write <= '1';
          else
            report "Stashing in queued";
            x1_queued_waddr <= address;
            x1_queued_wdata <= wdata;
            x1_queued_wdata_hi <= wdata_hi;
            x1_queued_wen_lo <= wen_lo;
            x1_queued_wen_hi <= wen_hi;
            x1_queued_write <= '1';
          end if;
        end if;
      end if;
    end if;
  end process;

  process (clock325) is
    variable clock_status_vector : unsigned(4 downto 0);
  begin
    -- Optionally delay HR_CLK by 1/2 an 160MHz clock cycle
    -- (actually just by optionally inverting it)
    if rising_edge(clock325) then
      x4_hr_clock_phase <= x4_hr_clock_phase + 1;
      -- Changing at the end of a phase cycle prevents us having any
      -- problematically short clock pulses when it matters.
      if x4_hr_clock_phase="111" then
        x4_hr_clk_fast_current <= x2_hr_clk_fast;
        x4_hr_clk_phaseshift_current <= x2_hr_clk_phaseshift;
        if x2_hr_clk_fast /= x4_hr_clk_fast_current or x4_hr_clk_phaseshift_current /= x2_hr_clk_phaseshift then
          report "Updating hr_clock_fast to " & std_logic'image(x2_hr_clk_fast)
            & ", x2_hr_clk_phaseshift to " & std_logic'image(x2_hr_clk_phaseshift);
        end if;
      end if;

      -- Only change clock mode when safe to do so
      clock_status_vector(4) := x4_hr_clk_fast_current;
      clock_status_vector(3) := x4_hr_clk_phaseshift_current;
      clock_status_vector(2 downto 0) := x4_hr_clock_phase;
      case clock_status_vector is
        -- Slow clock rate, no phase shift
        when "00000" => hr_clk_p <= '0';
        when "00001" => hr_clk_p <= '0';
        when "00010" => hr_clk_p <= '0';
        when "00011" => hr_clk_p <= '0';
        when "00100" => hr_clk_p <= '1';
        when "00101" => hr_clk_p <= '1';
        when "00110" => hr_clk_p <= '1';
        when "00111" => hr_clk_p <= '1';

        -- Slow clock rate, with phase shift = bring forward tick by 1/2 a cycle
        when "01000" => hr_clk_p <= '0';
        when "01001" => hr_clk_p <= '0';
        when "01010" => hr_clk_p <= '1';
        when "01011" => hr_clk_p <= '1';
        when "01100" => hr_clk_p <= '1';
        when "01101" => hr_clk_p <= '1';
        when "01110" => hr_clk_p <= '0';
        when "01111" => hr_clk_p <= '0';

        -- Fast clock rate, no phase shift
        when "10000" => hr_clk_p <= '0';
        when "10001" => hr_clk_p <= '0';
        when "10010" => hr_clk_p <= '1';
        when "10011" => hr_clk_p <= '1';
        when "10100" => hr_clk_p <= '0';
        when "10101" => hr_clk_p <= '0';
        when "10110" => hr_clk_p <= '1';
        when "10111" => hr_clk_p <= '1';

        -- Fast clock rate, with phase shift
        when "11000" => hr_clk_p <= '0';
        when "11001" => hr_clk_p <= '1';
        when "11010" => hr_clk_p <= '1';
        when "11011" => hr_clk_p <= '0';
        when "11100" => hr_clk_p <= '0';
        when "11101" => hr_clk_p <= '1';
        when "11110" => hr_clk_p <= '1';
        when "11111" => hr_clk_p <= '0';

        when others => hr_clk_p <= '0';
      end case;
    end if;
  end process;

  process (clock163) is
  begin
    if rising_edge(clock163) then
      x2_hr_clock_phase165 <= x2_hr_clock_phase165 + 1;

      x2_cycle_count <= x2_cycle_count + 1;

      if x2_read_request_delatch='1' and x1_read_request_latch='0' then
        x2_read_request_delatch <= '0';
      end if;

      x2_hyperram_access_address_read_time_adjusted <= to_unsigned(to_integer(x2_hyperram_access_address(2 downto 0))+C_READ_TIME_ADJUST,6);
      x2_seven_plus_read_time_adjust <= to_unsigned(7 + C_READ_TIME_ADJUST,6);

      if x1_write_collect0_address = x2_background_write_next_address then
        x2_background_write_next_address_matches_collect0 <= '1';
      else
        x2_background_write_next_address_matches_collect0 <= '0';
      end if;

      if x2_data_ready_strobe_hold = '0' then
        data_ready_strobe <= '0';
      else
        report "holding data_ready_strobe for an extra cycle";
        data_ready_strobe <= '1';
      end if;
      x2_data_ready_strobe_hold <= '0';

      -- Keep read request when required
      x2_read_request_held <= read_request;
      x2_write_request_held <= write_request;

      if x2_start_delay_expired='0' then
        x2_start_delay_counter <= x2_start_delay_counter - 1;
        if x2_start_delay_counter = 0 then
          x2_start_delay_expired <= '1';
          state <= WriteSetup;
        end if;
      end if;

      case state is
        when StartupDelay =>
          null;
        when ReadAbort =>
          -- Make sure we don't abort a read so quickly, that we allow
          -- glitching of clock line with clock phase shifting
          hr_cs0 <= '1';
          state <= Idle;
        when Idle =>
          hr_d_oe <= '0';

          x2_read_request_held <= '0';
          x2_write_request_held <= '0';

          x2_busy_internal <= '0';

          x2_is_expected_to_respond <= x1_ram_normalfetch;

          -- All commands need the clock offset by 1/2 cycle
          x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;
          x2_hr_clk_fast <= '1';

          x2_pause_phase <= '0';
          x2_countdown_timeout <= '0';

          -- Clear write buffer flags when they are empty
          if x1_write_collect0_dispatchable = '0' then
            x2_write_collect0_toolate <= '0';
            x2_write_collect0_flushed <= '0';
          end if;

          if x2_rwr_counter /= to_unsigned(0,8) then
            x2_rwr_counter <= x2_rwr_counter - 1;
            hr_d_out <= x"bb";
            hr_d_oe <= '1';
          end if;
          if x2_rwr_counter = to_unsigned(1,8) then
            x2_rwr_waiting <= '0';
          end if;

          -- Phase 101 guarantees that the clock base change will happen
          -- within the comming clock cycle
          if x2_rwr_waiting='0' and x2_hr_clock_phase165 = "10" then
            if x1_request_toggle /= x2_last_request_toggle and x1_write_collect0_dispatchable='0' then
              x2_ram_reading_held <= '1';

              report "Waiting to start read";
              x2_request_accepted <= x1_request_toggle;
              x2_last_request_toggle <= x1_request_toggle;
              state <= ReadSetup;
              x2_busy_internal <= '1';
            elsif x1_write_collect0_dispatchable = '1' then
              -- Do background write.
              x2_busy_internal <= '0';
              x2_request_accepted <= x1_request_toggle;
              x2_is_expected_to_respond <= false;

              report "DISPATCH: Writing out collect0 @ $" & to_hstring(x1_write_collect0_address&"000");

              x2_write_collect0_flushed <= '0';
              x2_write_collect0_toolate <= '0';
              x2_background_write_next_address <= x1_write_collect0_address;
              x2_background_write_next_address_matches_collect0 <= '1';
              x2_background_write <= '1';
              x2_background_write_fetch <= '1';
              x2_config_reg_write <= x1_write_collect0_address(25);

              -- Prepare command vector
              x2_hr_command(47) <= '0'; -- WRITE
              x2_hr_command(46) <= x1_write_collect0_address(25); -- Memory, not register space
              x2_hr_command(45) <= '1'; -- linear
              x2_hr_command(44 downto 35) <= (others => '0'); -- unused upper address bits
              x2_hr_command(15 downto 3) <= (others => '0'); -- reserved bits
              x2_hr_command(34 downto 16) <= x1_write_collect0_address(22 downto 4);
              x2_hr_command(2) <= x1_write_collect0_address(3);
              x2_hr_command(1 downto 0) <= "00";
              hr_reset <= '1'; -- active low reset

              x2_hyperram_access_address(26 downto 3) <= x1_write_collect0_address;
              x2_hyperram_access_address(2 downto 0) <= (others => '0');

              x2_ram_reading_held <= '0';
              state <= StartBackgroundWrite;

              if x1_write_collect0_address(25)='1' then
                x2_countdown <= 6 + 1;
              else
                x2_countdown <= 6;
              end if;
              x2_countdown_is_zero <= '0';
            else
              x2_busy_internal <= '0';
              x2_request_accepted <= x1_request_toggle;
            end IF;
            -- Release CS line between transactions
            hr_cs0 <= '1';
          end if;

        when StartBackgroundWrite =>
          report "in StartBackgroundWrite to synchronise with clock";
          x2_pause_phase <= '0';
          state <= HyperRAMOutputCommandSlow;
          x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;
          x2_hr_clk_fast <= '0';

        when ReadSetup =>
          report "Setting up to read $" & to_hstring(x1_ram_address) & " ( address = $" & to_hstring(address) & ")";

          -- Prepare command vector
          x2_hr_command(47) <= '1'; -- READ
          x2_hr_command(46) <= x1_ram_address(25); -- Memory address space (1) / Register address space select (0) ?
          x2_hr_command(45) <= '1'; -- Linear access (not wrapped)
          x2_hr_command(44 downto 37) <= (others => '0'); -- unused upper address bits
          x2_hr_command(34 downto 16) <= x1_ram_address(22 downto 4);
          x2_hr_command(15 downto 3) <= (others => '0'); -- reserved bits
          if x1_ram_address(25) = '0' then
            -- Always read on 8 byte boundaries, and read a full cache line
            x2_hr_command(2) <= x1_ram_address(3);
            x2_hr_command(1 downto 0) <= "00";
          else
            -- Except that register reads are weird: They read the same 2 bytes
            -- over and over again, so we have to make it set bit 0 of the CA
            -- for the "odd" registers"
            x2_hr_command(2 downto 1) <= "00";
            x2_hr_command(0) <= x1_ram_address(3);
          end if;

          x2_hyperram_access_address <= x1_ram_address;

          hr_reset <= '1'; -- active low reset
          x2_pause_phase <= '0';

          state <= HyperRAMOutputCommandSlow;
          x2_hr_clk_fast <= '0';
          x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;

          x2_countdown <= 6;
          x2_config_reg_write <= '0';
          x2_countdown_is_zero <= '0';

        when WriteSetup =>
          report "Preparing x2_hr_command etc for write to $" & to_hstring(x1_ram_address);

          x2_background_write_count <= 2;
          x2_background_write <= '0';
          x2_config_reg_write <= x1_ram_address(25);

          -- Prepare command vector
          -- As HyperRAM addresses on 16bit boundaries, we shift the address
          -- down one bit.
          x2_hr_command(47) <= '0'; -- WRITE
          x2_hr_command(46) <= x1_ram_address(25); -- Memory, not register space
          x2_hr_command(45) <= '1'; -- linear
          x2_hr_command(44 downto 35) <= (others => '0'); -- unused upper address bits
          x2_hr_command(15 downto 3) <= (others => '0'); -- reserved bits
          x2_hr_command(34 downto 16) <= x1_ram_address(22 downto 4);
          x2_hr_command(2 downto 0) <= x1_ram_address(3 downto 1);

          hr_reset <= '1'; -- active low reset

          x2_hyperram_access_address <= x1_ram_address;

          x2_pause_phase <= '0';

          if x2_start_delay_expired = '1' then
            state <= HyperRAMOutputCommandSlow;
            x2_hr_clk_fast <= '0';
            x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;
          end if;

          if x1_ram_address(25)='1' then
            x2_countdown <= 6 + 1;
          else
            x2_countdown <= 6;
          end if;
          x2_countdown_is_zero <= '0';

        when HyperRAMOutputCommandSlow =>
          report "Writing command, x2_hyperram_access_address=$" & to_hstring(x2_hyperram_access_address);
          report "x2_hr_command = $" & to_hstring(x2_hr_command);
          -- Call HyperRAM to attention
          hr_cs0 <= '0';
          hr_rwds_oe <= '0';

          x2_pause_phase <= not x2_pause_phase;

          if x2_pause_phase='1' then
            x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;

            if x2_countdown_timeout='1' then
              -- Finished shifting out
              if x2_ram_reading_held = '1' then
                -- Reading: We can just wait until hr_rwds has gone low, and then
                -- goes high again to indicate the first data byte
                x2_countdown <= 63;
                x2_countdown_is_zero <= '0';
                x2_hr_rwds_high_seen <= '0';
                x2_countdown_timeout <= '0';
                x2_pause_phase <= '1';
                x2_hr_clk_fast <= '0';
                state <= HyperRAMReadWaitSlow;
              elsif x2_config_reg_write='1' and x2_ram_reading_held='0' then
                -- Config register write.
                -- These are a bit weird, as they have no latency, and all 16
                -- bits have to get written at once.  So we will have 2 buffer
                -- registers that get setup, and then ANY write to the register
                -- area will write those values, which we have done by shifting
                -- those through and sending 48+16 bits instead of the usual
                -- 48.
                if x2_background_write='1' then
                  x2_write_collect0_flushed <= '1';
                end if;

                report "Finished writing config register";
                state <= HyperRAMFinishWriting;
              else
                -- Writing to memory, so count down the correct number of cycles;
                -- Initial latency is reduced by 2 cycles for the last bytes
                -- of the access command, and by 1 more to cover state
                -- machine latency
                x2_countdown <= to_integer(C_WRITE_LATENCY);
                x2_countdown_is_zero <= '0';

                -- We are not just about ready to start writing, so mark the
                -- write buffer as too late to be added to, because we will
                -- snap-shot it in a moment.
                if x2_background_write = '1' then
                  x2_background_write_count <= 4 + 2;
                  x2_write_continues <= C_WRITE_CONTINUES_MAX;
                  x2_write_collect0_toolate <= '1';
                  x2_write_collect0_flushed <= '0';
                end if;
                x2_countdown_timeout <= '0';
                x2_hr_clk_fast <= '0';
                state <= HyperRAMDoWriteSlow;
              end if;
            end if;

          else -- if x2_pause_phase='1' then

            -- Toggle data while clock steady
            report "Presenting x2_hr_command byte on hr_d = $" & to_hstring(x2_hr_command(47 downto 40))
              & ", x2_countdown = " & integer'image(x2_countdown);

            hr_d_out <= x2_hr_command(47 downto 40);
            hr_d_oe <= '1';
            x2_hr_command(47 downto 8) <= x2_hr_command(39 downto 0);

            -- Also shift out config register values, if required
            if x2_config_reg_write='1' and x2_ram_reading_held='0' then
              report "shifting in conf value $" & to_hstring(x2_conf_buf0);
              x2_hr_command(7 downto 0) <= x2_conf_buf0;
              x2_conf_buf0 <= x2_conf_buf1;
              x2_conf_buf1 <= x2_conf_buf0;
            else
              x2_hr_command(7 downto 0) <= x"00";
            end if;

            report "Writing command byte $" & to_hstring(x2_hr_command(47 downto 40));

            if x2_countdown = 3 and x2_config_reg_write='1' then
              if x2_background_write='1' then
                x2_write_collect0_toolate <= '1';
              end if;
            end if;

            if x2_countdown = 3 and (x2_config_reg_write='0' or x2_ram_reading_held='1') then
              x2_extra_latency <= hr_rwds_in;
              if hr_rwds_in = '1' then
                report "Applying extra latency";
              end if;
            end if;

            if x2_countdown = 1 then
              x2_countdown_is_zero <= '1';
            end if;

            if x2_countdown /= 0 then
              x2_countdown <= x2_countdown - 1;
            else
              report "asserting x2_countdown_timeout";
              x2_countdown_timeout <= '1';
            end if;
          end if;
          x2_byte_phase <= to_unsigned(0,6);
          x2_write_byte_phase <= '0';

        when HyperRAMDoWriteSlow =>
          x2_pause_phase <= not x2_pause_phase;

          if x2_background_write_count = 0 and x2_pause_phase = '0' then
            if x2_write_continues /= 0 and x2_background_chained_write='1' then
              x2_background_write_fetch <= '1';
            else
              report "WRITECONTINUE: No continuation. Terminating write.";
              x2_countdown_timeout <= '1';
            end if;
          end if;

          report "WRITE: LatencyWait state, bg_wr=" & std_logic'image(x2_background_write)
            & ", count=" & integer'image(x2_background_write_count)
            & ", x2_background_write_fetch = " & std_logic'image(x2_background_write_fetch)
            & ", x2_background_write_valids = " & to_string(x2_background_write_valids)
            & ", x1_write_blocked=" & std_logic'image(x1_write_blocked);

          -- Now snap-shot the write buffer data, and mark the slot as flushed
          if x2_background_write = '1' and x2_background_write_next_address_matches_collect0 = '1' then
            x2_background_chained_write <= '1';
          else
            x2_background_chained_write <= '0';

            if x2_hr_clock_phase165="11" and (x2_background_write_valids = "00000000")
              and (read_request='1' or write_request='1' or x1_write_blocked='1') then
              report "LatencyWait: Aborting tail of background write due to incoming job/x1_write_blocked";
              state <= HyperRAMFinishWriting;
            end if;
          end if;

          if x2_background_write_fetch = '1' then
            report "WRITE: Doing fetch of background write data";
            x2_background_write_fetch <= '0';
            x2_background_write_next_address <= x2_background_write_next_address + 1;
            x2_write_continues <= x2_write_continues - 1;
            x2_background_write_count <= 7;
            if x2_background_write_next_address_matches_collect0 = '1' then
              report "WRITE: x2_background_write_data copied from write_collect0 (@ $"
                & to_hstring(x1_write_collect0_address&"000")
                & "). Valids = " & to_string(x1_write_collect0_valids)
                & ", next addr was $" & to_hstring(x2_background_write_next_address&"000");

              x2_background_write_next_address <= x1_write_collect0_address + 1;
              x2_background_write_next_address_matches_collect0 <= '0';
              x2_background_write_data   <= x1_write_collect0_data;
              x2_background_write_valids <= x1_write_collect0_valids;
              x2_write_collect0_flushed  <= '1';
            else
              report "WRITE: Write is not chained.";
              x2_background_chained_write <= '0';
            end if;
          end if;

          if x2_pause_phase = '1' then
            x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;
            if x2_countdown_timeout = '1' then
              state <= HyperRAMFinishWriting;
            end if;
          else
            -- Begin write mask pre-amble
            if x2_ram_reading_held = '0' and x2_countdown = 2 then
              hr_rwds_out <= '0';
              hr_rwds_oe <= '1';
              hr_d_out <= x"BE"; -- "before" data byte
              hr_d_oe <= '1';
            end if;

            if x2_countdown /= 0 then
              x2_countdown <= x2_countdown - 1;
            end if;

            if x2_countdown = 1 then
              x2_countdown_is_zero <= '1';
            end if;

            if x2_countdown_is_zero = '1' then
              if x2_extra_latency='1' then
                report "Waiting 6 more cycles for extra latency";
                x2_extra_latency <= '0';
                x2_countdown <= to_integer(C_EXTRA_WRITE_LATENCY);
                x2_countdown_is_zero <= '0';
              else
                report "Presenting hr_d with ram_wdata or background data";
                if x2_background_write='1' then
                  report "WRITE: Writing background byte $" & to_hstring(x2_background_write_data(0))
                    & ", valids= " & to_string(x2_background_write_valids)
                    & ", background words left = " & integer'image(x2_background_write_count);
                  hr_d_out <= x2_background_write_data(0);
                  hr_d_oe <= '1';

                  x2_background_write_data(0) <= x2_background_write_data(1);
                  x2_background_write_data(1) <= x2_background_write_data(2);
                  x2_background_write_data(2) <= x2_background_write_data(3);
                  x2_background_write_data(3) <= x2_background_write_data(4);
                  x2_background_write_data(4) <= x2_background_write_data(5);
                  x2_background_write_data(5) <= x2_background_write_data(6);
                  x2_background_write_data(6) <= x2_background_write_data(7);
                  x2_background_write_data(7) <= x"00";

                  hr_rwds_out <= not x2_background_write_valids(0);
                  hr_rwds_oe <= '1';
                  x2_background_write_valids(0 to 6) <= x2_background_write_valids(1 to 7);
                  x2_background_write_valids(7) <= '0';
                else
                  -- XXX Doesn't handle 16-bit writes properly. But that's
                  -- okay, as they are only supported with the cache and
                  -- write-collecting, anyway.
                  hr_d_out <= (others => '0');
                  hr_d_oe <= '1';
                  hr_rwds_out <= x2_hyperram_access_address(0) xor x2_write_byte_phase;
                  hr_rwds_oe <= '1';
                end if;

                -- Finish resetting write collectors when chaining
                if x1_write_collect0_dispatchable='0' and x2_write_collect0_flushed='1' and x2_write_collect0_toolate='1' then
                  report "WRITECONTINUE: Resetting collect0";
                  x2_write_collect0_flushed <= '0';
                  x2_write_collect0_toolate <= '0';
                end if;

                -- Write byte
                x2_write_byte_phase <= '1';
                if x2_background_write='0' then
                  hr_d_out <= x"aa";
                  hr_d_oe <= '1';
                  if x2_background_write_count /= 0 then
                    x2_background_write_count <= x2_background_write_count - 1;
                  else
                    state <= HyperRAMFinishWriting;
                  end if;
                else
                  report "WRITE: Decrementing x2_background_write_count from " & integer'image(x2_background_write_count)
                    & ", x2_write_continues = " & integer'image(x2_write_continues);
                  if x2_background_write_count /= 0 then
                    x2_background_write_count <= x2_background_write_count - 1;
                    if x2_background_write_count = 3 and x2_write_continues /= 0 then
                      report "WRITECONTINUE: Checking for chained writes (" & integer'image(x2_write_continues) & " more continues allowed)";
                      -- Get ready to commit next write block, if one is there
                      if x2_write_continues /= 0 and x2_write_collect0_toolate='0' and x2_write_collect0_flushed = '0'
                        and x2_background_write_next_address_matches_collect0='1' then
                        report "WRITECONTINUE: Marking collect0 @ $" & to_hstring(x1_write_collect0_address&"000") & " for chained write.";
                        x2_write_collect0_toolate <= '1';
                      end if;
                    end if;
                  end if;
                end if;
              end if;
            end if;
          end if;

        when HyperRAMFinishWriting =>
          -- Mask writing from here on.
          hr_cs0 <= '1';
          hr_rwds_oe <= '0';
          hr_d_out <= x"FA"; -- "after" data byte
          hr_d_oe <= '1';
          x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;
          x2_rwr_counter <= C_RWR_DELAY;
          x2_rwr_waiting <= '1';
          report "returning to idle";
          state <= Idle;

        when HyperRAMReadWaitSlow =>
          hr_rwds_oe <= '0';
          hr_d_oe <= '0';

          x2_pause_phase <= not x2_pause_phase;

          -- After we have read the first 8 bytes, we know that we are no longer
          -- required to provide any further direct output, so clear the
          -- flag, so that the above logic can terminate a pre-fetch when required.
          if x2_byte_phase = 8 then
            report "DISPATCH: Clearing x2_is_expected_to_respond";
            x2_is_expected_to_respond <= false;
          end if;

          if x2_pause_phase = '0' then
            x2_hr_clk_phaseshift <= C_READ_PHASE_SHIFT;
            if x2_countdown_is_zero = '0' then
              x2_countdown <= x2_countdown - 1;
            end if;
            if x2_countdown = 1 then
              x2_countdown_is_zero <= '1';
            end if;

            if x2_countdown_is_zero = '1' then
              -- Timed out waiting for read -- so return anyway, rather
              -- than locking the machine hard forever.
              rdata_hi <= x"DD";
              rdata <= x"DD";
              data_ready_strobe <= '1';
              x2_data_ready_strobe_hold <= '1';
              x2_rwr_counter <= C_RWR_DELAY;
              x2_rwr_waiting <= '1';
              report "returning to idle";
              state <= Idle;
              x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;
            end if;

            -- HyperRAM drives RWDS basically to follow the clock.
            -- But first valid data is when RWDS goes high, so we have to
            -- wait until we see it go high.
            x2_hr_rwds_high_seen <= hr_rwds_in;

            if hr_rwds_in = '1' or x2_hr_rwds_high_seen = '1' then
              if x2_byte_phase = x2_hyperram_access_address_read_time_adjusted then
                report "DISPATCH: Returning freshly read data = $" & to_hstring(hr_d_in);
                rdata <= hr_d_in;
                if rdata_16en='0' then
                  report "asserting data_ready_strobe on low byte";
                  data_ready_strobe <= '1';
                  x2_data_ready_strobe_hold <= '1';
                end if;
              end if;

              if x2_byte_phase = (x2_hyperram_access_address_read_time_adjusted+1) and (rdata_16en='1') then
                report "DISPATCH: Returning freshly read high-byte data = $" & to_hstring(hr_d_in);
                rdata_hi <= hr_d_in;

                report "asserting data_ready_strobe on high byte";
                data_ready_strobe <= '1';
                x2_data_ready_strobe_hold <= '1';
              end if;

              if x2_byte_phase = x2_seven_plus_read_time_adjust then
                x2_rwr_counter <= C_RWR_DELAY;
                x2_rwr_waiting <= '1';
                report "returning to idle";
                state <= Idle;
                hr_cs0 <= '1';
                x2_hr_clk_phaseshift <= C_WRITE_PHASE_SHIFT;
              else
                x2_byte_phase <= x2_byte_phase + 1;
              end if;
            end if;
          end if;
      end case;
    end if;
  end process;
end architecture synthesis;

