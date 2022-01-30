library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity hyperram_wrapper is
   port (
      clk_i               : in    std_logic; -- Main clock
      clk_x2_i            : in    std_logic; -- Physical I/O only
      clk_x4_i            : in    std_logic; -- Physical I/O only
      rst_i               : in    std_logic;

      -- Avalon Memory Map
      avm_write_i         : in    std_logic;
      avm_read_i          : in    std_logic;
      avm_address_i       : in    std_logic_vector(31 downto 0);
      avm_writedata_i     : in    std_logic_vector(15 downto 0);
      avm_byteenable_i    : in    std_logic_vector(1 downto 0);
      avm_burstcount_i    : in    std_logic_vector(7 downto 0);
      avm_readdata_o      : out   std_logic_vector(15 downto 0);
      avm_readdatavalid_o : out   std_logic;
      avm_waitrequest_o   : out   std_logic;

      -- HyperRAM device interface
      hr_resetn_o         : out   std_logic;
      hr_csn_o            : out   std_logic;
      hr_ck_o             : out   std_logic;
      hr_rwds_io          : inout std_logic;
      hr_dq_io            : inout unsigned(7 downto 0)
   );
end entity hyperram_wrapper;

architecture synthesis of hyperram_wrapper is

   signal read_request      : std_logic;
   signal write_request     : std_logic;
   signal address           : unsigned(26 downto 0);
   signal wdata             : unsigned(7 downto 0);
   signal wdata_hi          : unsigned(7 downto 0);
   signal wen_hi            : std_logic;
   signal wen_lo            : std_logic;
   signal rdata_hi          : unsigned(7 downto 0);
   signal rdata_16en        : std_logic;
   signal rdata             : unsigned(7 downto 0);
   signal data_ready_strobe : std_logic;
   signal busy              : std_logic;

   signal wait_for_read     : std_logic;

begin

   rdata_16en <= '1';

   p_convert : process (clk_i)
   begin
      if rising_edge(clk_i) then
         write_request <= '0';
         read_request  <= '0';
         if busy = '0' and write_request = '0' and wait_for_read = '0' then
            avm_waitrequest_o <= '0';
         end if;

         if avm_write_i = '1' and avm_waitrequest_o = '0' and busy = '0' then
            write_request     <= '1';
            address           <= unsigned(avm_address_i(25 downto 0)) & '0';
            wdata             <= unsigned(avm_writedata_i(7 downto 0));
            wdata_hi          <= unsigned(avm_writedata_i(15 downto 8));
            wen_hi            <= avm_byteenable_i(1);
            wen_lo            <= avm_byteenable_i(0);
            avm_waitrequest_o <= '1';
         end if;

         if avm_read_i = '1' and avm_waitrequest_o = '0' and busy = '0' then
            read_request      <= '1';
            address           <= unsigned(avm_address_i(25 downto 0)) & '0';
            avm_waitrequest_o <= '1';
            wait_for_read     <= '1';
         end if;

         avm_readdata_o(15 downto 8) <= std_logic_vector(rdata_hi);
         avm_readdata_o( 7 downto 0) <= std_logic_vector(rdata);
         avm_readdatavalid_o         <= data_ready_strobe;

         if data_ready_strobe = '1' then
            wait_for_read <= '0';
         end if;

         if rst_i = '1' then
            avm_waitrequest_o <= '1';
            wait_for_read     <= '0';
         end if;
      end if;
   end process p_convert;


   i_hyperram_mega65 : entity work.hyperram_mega65
   port map (
      pixelclock                                  => clk_i,
      clock163                                    => clk_x2_i,
      clock325                                    => clk_x4_i,
      request_counter                             => open,
      read_request                                => read_request,
      write_request                               => write_request,
      address                                     => address,
      wdata                                       => wdata,
      wdata_hi                                    => wdata_hi,
      wen_hi                                      => wen_hi,
      wen_lo                                      => wen_lo,
      rdata_hi                                    => rdata_hi,
      rdata_16en                                  => rdata_16en,
      rdata                                       => rdata,
      data_ready_strobe                           => data_ready_strobe,
      busy                                        => busy,
      current_cache_line                          => open,
      current_cache_line_address                  => open,
      current_cache_line_valid                    => open,
      hr_d                                        => hr_dq_io,
      hr_rwds                                     => hr_rwds_io,
      hr_reset                                    => hr_resetn_o,
      hr_clk_p                                    => hr_ck_o,
      hr_cs0                                      => hr_csn_o
   ); -- i_hyperram_mega65

end architecture synthesis;

