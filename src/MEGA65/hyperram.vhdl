library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use Std.TextIO.all;
--use work.debugtools.all;
use work.cputypes.all;

entity hyperram_mega65 is
  Port ( pixelclock : in STD_LOGIC; -- For slow devices bus interface is
         -- actually on pixelclock to reduce latencies
         -- Also pixelclock is the natural clock speed we apply to the HyperRAM.
         clock163 : in std_logic; -- Used for fast clock for HyperRAM
         clock325 : in std_logic; -- Used for fast clock for HyperRAM SERDES units

         -- Simple counter for number of requests received
         request_counter : out std_logic := '0';

         read_request : in std_logic;
         write_request : in std_logic;
         address : in unsigned(26 downto 0);
         wdata : in unsigned(7 downto 0);

         -- Optional 16-bit interface (for Amiga core use)
         -- (That it is optional, is why the write_en is inverted for the
         -- low-byte).
         -- 16-bit transactions MUST occur on an even numbered address, or
         -- else expect odd and horrible things to happen.
         wdata_hi : in unsigned(7 downto 0) := x"00";
         wen_hi : in std_logic := '0';
         wen_lo : in std_logic := '1';
         rdata_hi : out unsigned(7 downto 0);
         rdata_16en : in std_logic := '0';         -- set this high to be able
                                                   -- to read 16-bit values
         rdata : out unsigned(7 downto 0);
         data_ready_strobe : out std_logic := '0';
         busy : out std_logic := '0';

         -- Export current cache line for speeding up reads from slow_devices controller
         -- by skipping the need to hand us the request and get the response back.
         current_cache_line : out cache_row_t := (others => (others => '0'));
         current_cache_line_address : inout unsigned(26 downto 3) := (others => '0');
         current_cache_line_valid : out std_logic := '0';

         hr_d : inout unsigned(7 downto 0) := (others => 'Z'); -- Data/Address
         hr_rwds : inout std_logic := 'Z'; -- RW Data strobe
         hr_reset : out std_logic := '1'; -- Active low RESET line to HyperRAM
         hr_clk_p : out std_logic := '1';
         hr_cs0 : out std_logic := '1'
         );
end hyperram_mega65;

architecture gothic of hyperram_mega65 is

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
  constant rwr_delay : unsigned(7 downto 0) := to_unsigned(1,8);
  signal x2_rwr_counter : unsigned(7 downto 0) := (others => '0');
  signal x2_rwr_waiting : std_logic := '0';

  signal x2_current_cache_line_drive : cache_row_t := (others => (others => '0'));
  signal x2_current_cache_line_address_drive : unsigned(26 downto 3) := (others => '0');

  signal state : state_t := StartupDelay;
  signal x2_busy_internal : std_logic := '1';
  signal hr_command : unsigned(47 downto 0);

  -- Initial transaction is config register write
  signal x2_config_reg_write : std_logic := '1';
  signal x1_ram_address : unsigned(26 downto 0) :=
    "010000000000001000000000000"; -- = bottom 27 bits of x"A001000";
  constant ram_wdata : unsigned(7 downto 0) := x"00";
  constant ram_wdata_hi : unsigned(7 downto 0) := x"00";
  constant ram_wdata_enlo : std_logic := '0';
  constant ram_wdata_enhi : std_logic := '0';
  signal x1_ram_reading : std_logic := '0';
  signal x2_ram_reading_held : std_logic := '0';

  signal x2_ram_wdata_drive : unsigned(7 downto 0) := x"00";
  signal x2_ram_wdata_hi_drive : unsigned(7 downto 0) := x"00";
  signal x2_ram_wdata_enlo_drive : std_logic := '0';
  signal x2_ram_wdata_enhi_drive : std_logic := '0';
  signal x2_ram_address_matches_current_cache_line_address : std_logic := '0';

  -- We want to set config register 0 to $ffe6, to enable variable latency
  -- and 3 cycles instead of 6 for latency. This speeds up writing almost 2x.
  -- But at 80MHz instead of 40MHz bus, we have to increase the latency from
  -- 3 to 4 cycles to satisfy the 40ns minimum time requirement.
  -- This also sets the drive strength to the maximum, to get cleaner faster
  -- clock transitions. This fixes checkerboard read errors at 80MHz.

  signal x2_conf_buf0 : unsigned(7 downto 0) := x"ff";
  signal x2_conf_buf1 : unsigned(7 downto 0) := x"f6";
  constant conf_buf0_in : unsigned(7 downto 0) := x"ff";
  constant conf_buf1_in : unsigned(7 downto 0) := x"f6";
  constant conf_buf0_set : std_logic := '0';
  constant conf_buf1_set : std_logic := '0';
  signal x2_last_conf_buf0_set : std_logic := '0';
  signal x2_last_conf_buf1_set : std_logic := '0';

  -- 4 is correct for the part we have in the MEGA65, after we have set the
  -- config register to minimise latency.
  constant write_latency : unsigned(7 downto 0) := to_unsigned(5,8);
  -- And the matching extra latency is 5
  constant extra_write_latency : unsigned(7 downto 0) := to_unsigned(7,8);

  constant read_phase_shift : std_logic := '0';
  constant write_phase_shift : std_logic := '1';

  signal x2_countdown : integer range 0 to 63 := 0;
  signal x2_countdown_is_zero : std_logic := '1';
  signal x2_extra_latency : std_logic := '0';
  signal x2_countdown_timeout : std_logic := '0';

  signal x2_pause_phase : std_logic := '0';

  signal x2_data_ready_strobe_hold : std_logic := '0';

  signal x1_request_toggle : std_logic := '0';
  signal x2_request_accepted : std_logic := '0';
  signal x2_last_request_toggle : std_logic := '0';

  signal x2_byte_phase : unsigned(5 downto 0) := to_unsigned(0,6);
  signal x2_write_byte_phase : std_logic := '0';

  signal x2_cycle_count : integer := 0;

  -- Collect writes together to hide write latency
  signal x1_write_collect0_dispatchable : std_logic := '0';
  signal x1_write_collect0_address : unsigned(26 downto 3) := (others => '0');
  signal x1_write_collect0_valids : std_logic_vector(0 to 7) := (others => '0');
  signal x1_write_collect0_data : cache_row_t := ( others => x"00" );
  signal x2_write_collect0_toolate : std_logic := '0'; -- Set when its too late to
                                                    -- add more bytes to the write.
  signal x2_write_collect0_flushed : std_logic := '1';


  signal x2_is_expected_to_respond : boolean := false;
  signal x1_ram_normalfetch : boolean := false;

  constant current_cache_line_new_address : unsigned(26 downto 3) := (others => '0');
  signal x1_current_cache_line_update : cache_row_t := (others => (others => '0'));
  signal x1_current_cache_line_update_address : unsigned(26 downto 3) := (others => '0');
  constant current_cache_line_update_all : std_logic := '0';
  signal x1_current_cache_line_update_flags : std_logic_vector(0 to 7) := (others => '0');
  signal x2_last_current_cache_line_update_all : std_logic := '0';
  signal x2_last_current_cache_line_update_flags : std_logic_vector(0 to 7) := (others => '0');

  signal x1_fake_data_ready_strobe : std_logic := '0';
  constant fake_rdata : unsigned(7 downto 0) := x"00";
  constant fake_rdata_hi : unsigned(7 downto 0) := x"00";

  signal x1_request_counter_int : std_logic := '1';


  signal x2_hr_rwds_high_seen : std_logic := '0';

  signal x1_write_blocked : std_logic := '0';

  signal x2_background_write : std_logic := '0';
  signal x2_background_write_valids : std_logic_vector(0 to 7) := x"00";
  signal x2_background_write_data : cache_row_t := (others => (others => '0'));
  signal x2_background_write_count : integer range 0 to 7 := 0;
  signal x2_background_write_next_address : unsigned(26 downto 3) := (others => '0');
  signal x2_background_write_next_address_matches_collect0 : std_logic := '0';
  signal x2_background_chained_write : std_logic := '0';
  signal x2_background_write_fetch : std_logic := '0';
  signal x1_cache_row_update_address_changed : std_logic := '0';

  signal x2_write_continues : integer range 0 to 255 := 0;
  constant write_continues_max : integer range 0 to 255 := 16;

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
  signal x4_hr_clk_phaseshift_current : std_logic := '1';

  signal x2_hr_clk_fast : std_logic := '1';
  signal x4_hr_clk_fast_current : std_logic := '1';

  signal x2_hr_clock_phase165 : unsigned(1 downto 0) := "00";
  signal x4_hr_clock_phase : unsigned(2 downto 0) := "000";
  signal x4_hr_clock_phase_drive : unsigned(2 downto 0) := "111";

  constant read_time_adjust : integer range 0 to 255 := 0;
  signal x2_seven_plus_read_time_adjust : unsigned(5 downto 0) := "000000";
  signal x2_hyperram_access_address_read_time_adjusted : unsigned(5 downto 0) := "000000";

  signal x2_hyperram_access_address : unsigned(26 downto 0) := to_unsigned(0,27);

  signal x2_read_request_held : std_logic := '0';
  signal x2_write_request_held : std_logic := '0';

  signal x1_read_request_latch : std_logic := '0';
  signal x2_read_request_delatch : std_logic := '0';
  signal x1_write_request_latch : std_logic := '0';

begin
  process (pixelclock) is
  begin
    if rising_edge(pixelclock) then

      x1_cache_row_update_address_changed <= '0';

      if read_request='1' then
        x1_read_request_latch <= '1';
      end if;
      if write_request='1' then
        x1_write_request_latch <= '1';
      end if;
      if x2_read_request_delatch = '1' then
        x1_read_request_latch <= '0';
      end if;

      report "read_request=" & std_logic'image(read_request)
        & ", x2_read_request_held=" & std_logic'image(x2_read_request_held)
        & ", x2_write_request_held=" & std_logic'image(x2_write_request_held)
        & ", x1_read_request_latch=" & std_logic'image(x1_read_request_latch)
        & ", x1_write_request_latch=" & std_logic'image(x1_write_request_latch)
        & ", x2_busy_internal=" & std_logic'image(x2_busy_internal)
        & ", write_request=" & std_logic'image(write_request)
        & ", x1_request_toggle(last) = " & std_logic'image(x1_request_toggle) & "(" & std_logic'image(x2_last_request_toggle) & ")."
        & ", address=$" & to_hstring(address);

      -- Update short-circuit cache line
      -- (We don't change validity, since we don't know if it is
      -- valid or not).
      -- This has to happen IMMEDIATELY so that slow_devices doesn't
      -- accidentally read old data, while we are still scheduling the write.
      if address(26 downto 3) = current_cache_line_address(26 downto 3) then
        report "Requesting update of current_cache_line due to write. Value = $"
          & to_hstring(wdata) & ", byte offset = " & integer'image(to_integer(address(2 downto 0)));
        if wen_lo = '1' then
          x1_current_cache_line_update(to_integer(address(2 downto 0))) <= wdata;
          x1_current_cache_line_update_flags(to_integer(address(2 downto 0))) <=
            not x1_current_cache_line_update_flags(to_integer(address(2 downto 0)));
          x1_current_cache_line_update_address <= current_cache_line_address;
          end if;
        if wen_hi = '1' then
          x1_current_cache_line_update(to_integer(address(2 downto 0))+1) <= wdata_hi;
          x1_current_cache_line_update_flags(to_integer(address(2 downto 0))+1) <=
            not x1_current_cache_line_update_flags(to_integer(address(2 downto 0))+1);
          x1_current_cache_line_update_address <= current_cache_line_address;
        end if;
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

      x1_fake_data_ready_strobe <= '0';

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
      report "address = $" & to_hstring(address);
      if (read_request or x1_read_request_latch)='1' and x2_busy_internal='0' then
        report "Making read request for $" & to_hstring(address);
        -- Begin read request

        x1_read_request_latch <= '0';

        if x2_request_accepted = x1_request_toggle then
          -- Normal RAM read.
          report "x1_request_toggle flipped";
          x1_ram_reading <= '1';
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
        -- Begin write request
        -- Latch address and data

        x1_write_request_latch <= '0';

        -- Collect writes together for dispatch

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

        -- Update read cache structures when writing
        report "CACHE: Requesting update of cache due to write: $" & to_hstring(address) & " = $" & to_hstring(wdata);
        x1_cache_row_update_address_changed <= '1';
      end if;
    end if;
  end process;

  process (clock325) is
    variable clock_status_vector : unsigned(4 downto 0);
    begin
    -- Optionally delay HR_CLK by 1/2 an 160MHz clock cycle
    -- (actually just by optionally inverting it)
    if rising_edge(clock325) then
      x4_hr_clock_phase_drive <= x4_hr_clock_phase;
      x4_hr_clock_phase <= x4_hr_clock_phase + 1;
      -- Changing at the end of a phase cycle prevents us having any
      -- problematically short clock pulses when it matters.
      if x4_hr_clock_phase_drive="110" then
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
      report "clock phase vector = " & to_string(std_logic_vector(clock_status_vector));
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

      x2_hyperram_access_address_read_time_adjusted <= to_unsigned(to_integer(x2_hyperram_access_address(2 downto 0))+read_time_adjust,6);
      x2_seven_plus_read_time_adjust <= to_unsigned(7 + read_time_adjust,6);

      -- We run double the clock speed of the pixelclock area, so no request
      -- can come in during the extra drive cycle we use to update these values
      -- so as to improve the timing closure of the whole thing
      x2_ram_wdata_drive <= ram_wdata;
      x2_ram_wdata_hi_drive <= ram_wdata_hi;
      x2_ram_wdata_enlo_drive <= ram_wdata_enlo;
      x2_ram_wdata_enhi_drive <= ram_wdata_enhi;

      if x1_ram_address(26 downto 3) = current_cache_line_address(26 downto 3) then
        x2_ram_address_matches_current_cache_line_address <= '1';
      else
        x2_ram_address_matches_current_cache_line_address <= '0';
      end if;
      if x1_write_collect0_address = x2_background_write_next_address then
        x2_background_write_next_address_matches_collect0 <= '1';
      else
        x2_background_write_next_address_matches_collect0 <= '0';
      end if;


      -- Update short-circuit cache line
      -- (We don't change validity, since we don't know if it is
      -- valid or not).
      if x2_ram_address_matches_current_cache_line_address = '1' then
        if x2_ram_wdata_enlo_drive='1' then
          x2_current_cache_line_drive(to_integer(x2_hyperram_access_address(2 downto 0))) <= x2_ram_wdata_drive;
        end if;
        if x2_ram_wdata_enhi_drive='1' then
          x2_current_cache_line_drive(to_integer(x2_hyperram_access_address(2 downto 0))+1) <= x2_ram_wdata_hi_drive;
        end if;
      end if;


      current_cache_line <= x2_current_cache_line_drive;
      current_cache_line_address <= x2_current_cache_line_address_drive;
      current_cache_line_valid <= '0';

      if x2_data_ready_strobe_hold = '0' then
        if x1_fake_data_ready_strobe='1' then
          report "asserting data_ready_strobe via x1_fake_data_ready_strobe";
        end if;
        data_ready_strobe <= x1_fake_data_ready_strobe;
        if x1_fake_data_ready_strobe='1' then
          report "DISPATCH: holding data_ready_strobe via fake data = $" & to_hstring(fake_rdata);
          rdata <= fake_rdata;
          rdata_hi <= fake_rdata_hi;
        end if;
      else
        report "holding data_ready_strobe for an extra cycle";
        report "asserting data_ready_strobe";
        data_ready_strobe <= '1';
      end if;
      x2_data_ready_strobe_hold <= '0';

      -- HyperRAM state machine
      report "State = " & state_t'image(state) & " @ Cycle " & integer'image(x2_cycle_count)
        & ", x2_config_reg_write=" & std_logic'image(x2_config_reg_write);

      if conf_buf0_set /= x2_last_conf_buf0_set then
        x2_last_conf_buf0_set <= conf_buf0_set;
        x2_conf_buf0 <= conf_buf0_in;
      end if;
      if conf_buf1_set /= x2_last_conf_buf1_set then
        x2_last_conf_buf1_set <= conf_buf1_set;
        x2_conf_buf1 <= conf_buf1_in;
      end if;


      if current_cache_line_update_all = x2_last_current_cache_line_update_all then
        if x1_current_cache_line_update_address = x2_current_cache_line_address_drive then
          for i in 0 to 7 loop
            if x1_current_cache_line_update_flags(i) /= x2_last_current_cache_line_update_flags(i)  then
              report "CACHE: Driving update to current_cache_line byte " & integer'image(i)
                & ", value $" & to_hstring(x1_current_cache_line_update(i));
              x2_last_current_cache_line_update_flags(i) <= x1_current_cache_line_update_flags(i);
              x2_current_cache_line_drive(i) <= x1_current_cache_line_update(i);
            end if;
          end loop;
        end if;
      else
        report "DISPATCHER: Replacing current cache line with $" & to_hstring(current_cache_line_new_address&"000");
        x2_last_current_cache_line_update_all <= current_cache_line_update_all;
        x2_current_cache_line_address_drive <= current_cache_line_new_address;
        x2_current_cache_line_drive <= x1_current_cache_line_update;
        x2_last_current_cache_line_update_flags <= x1_current_cache_line_update_flags;
      end if;

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
          report "Tristating hr_d";
          hr_d <= (others => 'Z');

          x2_read_request_held <= '0';
          x2_write_request_held <= '0';

          x2_busy_internal <= '0';

          x2_is_expected_to_respond <= x1_ram_normalfetch;

          -- All commands need the clock offset by 1/2 cycle
          x2_hr_clk_phaseshift <= write_phase_shift;
          x2_hr_clk_fast <= '1';

          x2_pause_phase <= '0';
          x2_countdown_timeout <= '0';

          -- Clear write buffer flags when they are empty
          if x1_write_collect0_dispatchable = '0' then
            x2_write_collect0_toolate <= '0';
            x2_write_collect0_flushed <= '0';
          end if;

          -- Mark us ready for a new job, or pick up a new job
          report
            "r_t=" & std_logic'image(x1_request_toggle)
            & ", l_r_t=" & std_logic'image(x2_last_request_toggle)
            & ", x2_rwr_counter = " & integer'image(to_integer(x2_rwr_counter));

          if x2_rwr_counter /= to_unsigned(0,8) then
            x2_rwr_counter <= x2_rwr_counter - 1;
            hr_d <= x"bb";
          end if;
          if x2_rwr_counter = to_unsigned(1,8) then
            x2_rwr_waiting <= '0';
          end if;

          -- Phase 101 guarantees that the clock base change will happen
          -- within the comming clock cycle
          if x2_rwr_waiting='0' and  x2_hr_clock_phase165 = "10" then
            if (x1_request_toggle /= x2_last_request_toggle)
              -- Only commence reads AFTER all pending writes have flushed,
              -- to ensure cache coherence (there are corner-cases here with
              -- chained writes, block reads and other bits and pieces).
              and x1_write_collect0_dispatchable='0' then
              report "WAITING for job";
              x2_ram_reading_held <= x1_ram_reading;

              if x1_ram_reading = '1' then
                report "Waiting to start read";
                x2_request_accepted <= x1_request_toggle;
                x2_last_request_toggle <= x1_request_toggle;
                state <= ReadSetup;
                report "Accepting job";
                x2_busy_internal <= '1';
              else
                report "Waiting to start write";
                x2_request_accepted <= x1_request_toggle;
                x2_last_request_toggle <= x1_request_toggle;
                state <= WriteSetup;
                report "Accepting job";
                x2_busy_internal <= '1';
              end if;
            elsif (x1_write_collect0_dispatchable = '1') then
              -- Do background write.
              x2_busy_internal <= '0';
              x2_request_accepted <= x1_request_toggle;
              x2_is_expected_to_respond <= false;

              report "DISPATCH: Writing out collect0 @ $" & to_hstring(x1_write_collect0_address&"000");

              -- Mark the write buffer as being processed.
              x2_write_collect0_flushed <= '0';
              -- And that it is not (yet) too late to add extra bytes to the write.
              x2_write_collect0_toolate <= '0';

              x2_background_write_next_address <= x1_write_collect0_address;
              x2_background_write_next_address_matches_collect0 <= '1';
              x2_background_write <= '1';
              x2_background_write_fetch <= '1';

              x2_config_reg_write <= x1_write_collect0_address(25);

              -- Prepare command vector
              hr_command(47) <= '0'; -- WRITE
              hr_command(46) <= x1_write_collect0_address(25); -- Memory, not register space
              hr_command(45) <= '1'; -- linear
              hr_command(44 downto 35) <= (others => '0'); -- unused upper address bits
              hr_command(15 downto 3) <= (others => '0'); -- reserved bits
              hr_command(34 downto 16) <= x1_write_collect0_address(22 downto 4);
              hr_command(2) <= x1_write_collect0_address(3);
              hr_command(1 downto 0) <= "00";
              hr_reset <= '1'; -- active low reset


              x2_hyperram_access_address(26 downto 3) <= x1_write_collect0_address;
              x2_hyperram_access_address(2 downto 0) <= (others => '0');

              x2_ram_reading_held <= '0';

              -- This is the delay before we assert CS

              -- We have to use this intermediate stage to get the clock
              -- phase right.
              state <= StartBackgroundWrite;

              if x1_write_collect0_address(25)='1' then
                -- 48 bits of CA followed by 16 bit register value
                -- (we shift the buffered config register values out automatically)
                x2_countdown <= 6 + 1;
              else
                x2_countdown <= 6;
              end if;
              x2_countdown_is_zero <= '0';
            else
              report "Clearing x2_busy_internal";
              x2_busy_internal <= '0';
              x2_request_accepted <= x1_request_toggle;
            end IF;
            -- Release CS line between transactions
            report "Releasing hyperram CS lines";
            hr_cs0 <= '1';
          end if;

        when StartBackgroundWrite =>
          report "in StartBackgroundWrite to synchronise with clock";
          x2_pause_phase <= '0';
          state <= HyperRAMOutputCommandSlow;
          x2_hr_clk_phaseshift <= write_phase_shift;
          x2_hr_clk_fast <= '0';

        when ReadSetup =>
          report "Setting up to read $" & to_hstring(x1_ram_address) & " ( address = $" & to_hstring(address) & ")";

          -- Prepare command vector
          hr_command(47) <= '1'; -- READ
          -- Map actual RAM to bottom 32MB of 64MB space (repeated 4x)
          -- and registers to upper 32MB
--            hr_command(46) <= '1'; -- Memory address space (1) / Register
          hr_command(46) <= x1_ram_address(25); -- Memory address space (1) / Register
                                             -- address space select (0) ?
          hr_command(45) <= '1'; -- Linear access (not wrapped)
          hr_command(44 downto 37) <= (others => '0'); -- unused upper address bits
          hr_command(34 downto 16) <= x1_ram_address(22 downto 4);
          hr_command(15 downto 3) <= (others => '0'); -- reserved bits
          if x1_ram_address(25) = '0' then
            -- Always read on 8 byte boundaries, and read a full cache line
            hr_command(2) <= x1_ram_address(3);
            hr_command(1 downto 0) <= "00";
          else
            -- Except that register reads are weird: They read the same 2 bytes
            -- over and over again, so we have to make it set bit 0 of the CA
            -- for the "odd" registers"
            hr_command(2 downto 1) <= "00";
            hr_command(0) <= x1_ram_address(3);
          end if;


          x2_hyperram_access_address <= x1_ram_address;

          hr_reset <= '1'; -- active low reset
          x2_pause_phase <= '0';

          state <= HyperRAMOutputCommandSlow;
          x2_hr_clk_fast <= '0';
          x2_hr_clk_phaseshift <= write_phase_shift;

          x2_countdown <= 6;
          x2_config_reg_write <= '0';
          x2_countdown_is_zero <= '0';

        when WriteSetup =>

          report "Preparing hr_command etc for write to $" & to_hstring(x1_ram_address);

          x2_background_write_count <= 2;
          x2_background_write <= '0';

          x2_config_reg_write <= x1_ram_address(25);

          -- Prepare command vector
          -- As HyperRAM addresses on 16bit boundaries, we shift the address
          -- down one bit.
          hr_command(47) <= '0'; -- WRITE
          hr_command(46) <= x1_ram_address(25); -- Memory, not register space
          hr_command(45) <= '1'; -- linear

          hr_command(44 downto 35) <= (others => '0'); -- unused upper address bits
          hr_command(15 downto 3) <= (others => '0'); -- reserved bits

          hr_command(34 downto 16) <= x1_ram_address(22 downto 4);
          hr_command(2 downto 0) <= x1_ram_address(3 downto 1);

          hr_reset <= '1'; -- active low reset


          x2_hyperram_access_address <= x1_ram_address;

          x2_pause_phase <= '0';

          if x2_start_delay_expired = '1' then
            state <= HyperRAMOutputCommandSlow;
            x2_hr_clk_fast <= '0';
            x2_hr_clk_phaseshift <= write_phase_shift;
          end if;
          if x1_ram_address(25)='1' then
            -- 48 bits of CA followed by 16 bit register value
            -- (we shift the buffered config register values out automatically)
            x2_countdown <= 6 + 1;
          else
            x2_countdown <= 6;
          end if;
          x2_countdown_is_zero <= '0';

        when HyperRAMOutputCommandSlow =>
          report "Writing command, x2_hyperram_access_address=$" & to_hstring(x2_hyperram_access_address);
          report "hr_command = $" & to_hstring(hr_command);
          -- Call HyperRAM to attention
          hr_cs0 <= '0';

          hr_rwds <= 'Z';

          x2_pause_phase <= not x2_pause_phase;

          if x2_pause_phase='1' then
            x2_hr_clk_phaseshift <= write_phase_shift;

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
                x2_countdown <= to_integer(write_latency);
                -- XXX Doesn't work if write_latency(2) is $00
                x2_countdown_is_zero <= '0';


                -- We are not just about ready to start writing, so mark the
                -- write buffer as too late to be added to, because we will
                -- snap-shot it in a moment.
                if x2_background_write = '1' then
                  x2_background_write_count <= 4 + 2;
                  -- We know we can do upto 128 bytes at least per write,
                  -- before a refresh is required. So allow 16x8 byte writes to
                  -- be chained.
                  x2_write_continues <= write_continues_max;
                  x2_write_collect0_toolate <= '1';
                  x2_write_collect0_flushed <= '0';
                end if;
                x2_countdown_timeout <= '0';
                x2_hr_clk_fast <= '0';
                state <= HyperRAMDoWriteSlow;
              end if;
            end if;

          else

            -- Toggle data while clock steady
            report "Presenting hr_command byte on hr_d = $" & to_hstring(hr_command(47 downto 40))
              & ", x2_countdown = " & integer'image(x2_countdown);

            hr_d <= hr_command(47 downto 40);
            hr_command(47 downto 8) <= hr_command(39 downto 0);

            -- Also shift out config register values, if required
            if x2_config_reg_write='1' and x2_ram_reading_held='0' then
              report "shifting in conf value $" & to_hstring(x2_conf_buf0);
              hr_command(7 downto 0) <= x2_conf_buf0;
              x2_conf_buf0 <= x2_conf_buf1;
              x2_conf_buf1 <= x2_conf_buf0;
            else
              hr_command(7 downto 0) <= x"00";
            end if;

            report "Writing command byte $" & to_hstring(hr_command(47 downto 40));

            if x2_countdown = 3 and x2_config_reg_write='1' then
              if x2_background_write='1' then
                x2_write_collect0_toolate <= '1';
              end if;
            end if;

            if x2_countdown = 3 and (x2_config_reg_write='0' or x2_ram_reading_held='1') then
              x2_extra_latency <= hr_rwds;
              if (hr_rwds='1')
              then
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

          -- Fetch takes 2 cycles, so schedule one cycle before last read
          -- and shift, so that it happens after that last shift, but
          -- before it is needed again.
          if x2_background_write_count = 0 and x2_pause_phase = '0' then
            -- See if we have another write collect that we can
            -- continue with
            -- XXX We suspect that chained writes might be problematic on the
            -- external hyperram for some strange reason, so disable them.
            if x2_write_continues /= 0 and x2_background_chained_write='1' then
              if x2_background_write_fetch = '0' then
                report "WRITECONTINUE: Continuing write: Requesting fetch.";
                x2_background_write_fetch <= '1';
              end if;
            else
              report "WRITECONTINUE: No continuation. Terminating write.";
              report "asserting x2_countdown_timeout";
              x2_countdown_timeout <= '1';
            end if;
          end if;

          report "WRITE: LatencyWait state, bg_wr=" & std_logic'image(x2_background_write)
            & ", count=" & integer'image(x2_background_write_count)
            & ", x2_background_write_fetch = " & std_logic'image(x2_background_write_fetch)
            & ", x2_background_write_valids = " & to_string(x2_background_write_valids)
            & ", x1_write_blocked=" & std_logic'image(x1_write_blocked);

          -- Now snap-shot the write buffer data, and mark the slot as flushed
          if x2_background_write = '1' and
             (x2_background_write_next_address_matches_collect0 = '1')
          then
            if x2_background_chained_write = '0' then
              report "WRITE: x2_background_chained_write <= 1";
            end if;
            x2_background_chained_write <= '1';
          else
            if x2_background_chained_write = '1' then
              report "WRITE: x2_background_chained_write <= 0";
            end if;
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

              x2_background_write_data <= x1_write_collect0_data;
              x2_background_write_valids <= x1_write_collect0_valids;
              x2_write_collect0_flushed <= '1';
            else
              report "WRITE: Write is not chained.";
              x2_background_chained_write <= '0';
            end if;
          end if;

          if x2_pause_phase = '1' then
            x2_hr_clk_phaseshift <= write_phase_shift;
            if x2_countdown_timeout = '1' then
              report "Advancing to HyperRAMFinishWriting";
              state <= HyperRAMFinishWriting;
            end if;
          else

            report "latency x2_countdown = " & integer'image(x2_countdown);

            -- Begin write mask pre-amble
            if x2_ram_reading_held = '0' and x2_countdown = 2 then
              hr_rwds <= '0';
              hr_d <= x"BE"; -- "before" data byte
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
                -- If we were asked to wait for extra latency,
                -- then wait another 6 cycles.
                x2_extra_latency <= '0';
                x2_countdown <= to_integer(extra_write_latency);
                -- XXX Assumes extra_write_latency is not zero
                x2_countdown_is_zero <= '0';
              else
                -- Latency x2_countdown for writing is over, we can now
                -- begin writing bytes.

                -- HyperRAM works on 16-bit fundamental transfers.
                -- This means we need to have two half-cycles, and pick which
                -- one we want to write during.
                -- If RWDS is asserted, then the write is masked, i.e., won't
                -- occur.
                -- In this first

                report "Presenting hr_d with ram_wdata or background data";
                if x2_background_write='1' then
                  report "WRITE: Writing background byte $" & to_hstring(x2_background_write_data(0))
                    & ", valids= " & to_string(x2_background_write_valids)
                    & ", background words left = " & integer'image(x2_background_write_count);
                  hr_d <= x2_background_write_data(0);

                  x2_background_write_data(0) <= x2_background_write_data(1);
                  x2_background_write_data(1) <= x2_background_write_data(2);
                  x2_background_write_data(2) <= x2_background_write_data(3);
                  x2_background_write_data(3) <= x2_background_write_data(4);
                  x2_background_write_data(4) <= x2_background_write_data(5);
                  x2_background_write_data(5) <= x2_background_write_data(6);
                  x2_background_write_data(6) <= x2_background_write_data(7);
                  x2_background_write_data(7) <= x"00";

                  hr_rwds <= not x2_background_write_valids(0);
                  x2_background_write_valids(0 to 6) <= x2_background_write_valids(1 to 7);
                  x2_background_write_valids(7) <= '0';
                else
                  -- XXX Doesn't handle 16-bit writes properly. But that's
                  -- okay, as they are only supported with the cache and
                  -- write-collecting, anyway.
                  hr_d <= ram_wdata;
                  hr_rwds <= x2_hyperram_access_address(0) xor x2_write_byte_phase;
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
                  if x2_write_byte_phase = '0' and x2_hyperram_access_address(0)='1' then
                    hr_d <= x"ee"; -- even "masked" data byte
                  elsif x2_write_byte_phase = '1' and x2_hyperram_access_address(0)='0' then
                    hr_d <= x"0d"; -- odd "masked" data byte
                  end if;
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
                      report "WRITECONTINUE: Am looking for $" & to_hstring(x2_background_write_next_address&"000") &
                        ", options are 0:$" & to_hstring(x1_write_collect0_address&"000");
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
          hr_rwds <= 'Z';
          hr_d <= x"FA"; -- "after" data byte
          x2_hr_clk_phaseshift <= write_phase_shift;
          report "clk_queue <= '00'";
          x2_rwr_counter <= rwr_delay;
          x2_rwr_waiting <= '1';
          report "returning to idle";
          state <= Idle;

        when HyperRAMReadWaitSlow =>
          hr_rwds <= 'Z';
          report "Presenting tri-state on hr_d";
          hr_d <= (others => 'Z');

          x2_pause_phase <= not x2_pause_phase;

          -- After we have read the first 8 bytes, we know that we are no longer
          -- required to provide any further direct output, so clear the
          -- flag, so that the above logic can terminate a pre-fetch when required.
          if x2_byte_phase = 8 then
            report "DISPATCH: Clearing x2_is_expected_to_respond";
            x2_is_expected_to_respond <= false;
          end if;

          if x2_pause_phase = '1' then
            null;
          else
            x2_hr_clk_phaseshift <= read_phase_shift;
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
              rdata(0) <= '0';
              rdata(1) <= x2_busy_internal;
              report "asserting data_ready_strobe";
              data_ready_strobe <= '1';
              x2_data_ready_strobe_hold <= '1';
              x2_rwr_counter <= rwr_delay;
              x2_rwr_waiting <= '1';
              report "returning to idle";
              state <= Idle;
              x2_hr_clk_phaseshift <= write_phase_shift;
            end if;

            -- HyperRAM drives RWDS basically to follow the clock.
            -- But first valid data is when RWDS goes high, so we have to
            -- wait until we see it go high.

            if (hr_rwds='1')
            then
              x2_hr_rwds_high_seen <= '1';
            else
              x2_hr_rwds_high_seen <= '0';
--                if x2_hr_rwds_high_seen = '0' then
            --                report "DISPATCH saw hr_rwds go high at start of data stream";
--                end if;
            end if;
            if (hr_rwds='1')
              or (x2_hr_rwds_high_seen='1') then
              -- Data has arrived: Latch either odd or even byte
              -- as required.
--                  report "DISPATCH Saw read data = $" & to_hstring(hr_d);

              -- Quickly return the correct byte
              if x2_byte_phase = x2_hyperram_access_address_read_time_adjusted then
                report "DISPATCH: Returning freshly read data = $" & to_hstring(hr_d);
                rdata <= hr_d;
                report "hr_return='1'";
                report "hr_return='0'";
                if rdata_16en='0' then
                  report "asserting data_ready_strobe on low byte";
                  data_ready_strobe <= '1';
                  x2_data_ready_strobe_hold <= '1';
                end if;
              end if;

              if x2_byte_phase = (x2_hyperram_access_address_read_time_adjusted+1) and (rdata_16en='1') then
                report "DISPATCH: Returning freshly read high-byte data = $" & to_hstring(hr_d);
                rdata_hi <= hr_d;
                report "hr_return='1'";
                report "hr_return='0'";

                report "asserting data_ready_strobe on high byte";
                data_ready_strobe <= '1';
                x2_data_ready_strobe_hold <= '1';
              end if;

              report "x2_byte_phase = " & integer'image(to_integer(x2_byte_phase));
              if (x2_byte_phase = x2_seven_plus_read_time_adjust) then
                x2_rwr_counter <= rwr_delay;
                x2_rwr_waiting <= '1';
                report "returning to idle";
                state <= Idle;
                hr_cs0 <= '1';
                x2_hr_clk_phaseshift <= write_phase_shift;
              else
                x2_byte_phase <= x2_byte_phase + 1;
              end if;
            end if;
          end if;
      end case;
    end if;
  end process;
end gothic;


