library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity hyperram is
   port (
      clk_i               : in  std_logic; -- Main clock
      clk_x2_i            : in  std_logic; -- Physical I/O only
      clk_x2_del_i        : in  std_logic; -- Double frequency, phase shifted
      rst_i               : in  std_logic;

      -- Avalon Memory Map
      avm_write_i         : in  std_logic;
      avm_read_i          : in  std_logic;
      avm_address_i       : in  std_logic_vector(31 downto 0);
      avm_writedata_i     : in  std_logic_vector(15 downto 0);
      avm_byteenable_i    : in  std_logic_vector(1 downto 0);
      avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      avm_readdata_o      : out std_logic_vector(15 downto 0);
      avm_readdatavalid_o : out std_logic;
      avm_waitrequest_o   : out std_logic;

      -- HyperRAM device interface
      hr_resetn_o         : out std_logic;
      hr_csn_o            : out std_logic;
      hr_ck_o             : out std_logic;
      hr_rwds_in_i        : in  std_logic;
      hr_dq_in_i          : in  std_logic_vector(7 downto 0);
      hr_rwds_out_o       : out std_logic;
      hr_dq_out_o         : out std_logic_vector(7 downto 0);
      hr_rwds_oe_o        : out std_logic;
      hr_dq_oe_o          : out std_logic
   );
end entity hyperram;

architecture synthesis of hyperram is

begin

   i_hyperram_mega65 : entity work.hyperram_mega65
   generic map (
      in_simulation => false
   )
   port map (
      pixelclock                                  => clk_i,
      clock163                                    => clk_x2_i,
      clock325                                    => clk_x4_i,
      request_counter                             => open,
      read_request                                => avm_read_i,
      write_request                               => avm_write_i,
      address                                     => avm_address_i(26 downto 0),
      wdata                                       => avm_writedata_i(7 downto 0),
      wdata_hi                                    => avm_writedata_i(15 downto 8),
      wen_hi                                      => avm_byteenable_i(1),
      wen_lo                                      => avm_byteenable_i(0),
      rdata_hi                                    => avm_readdata_o(15 downto 8),
      rdata_16en                                  => '1',
      rdata                                       => avm_readdata_o(7 downto 0),
      data_ready_strobe                           => avm_readdatavalid_o,
      busy                                        => avm_waitrequest_o,
      current_cache_line                          => open,
      current_cache_line_address                  => (others => 'Z'),
      current_cache_line_valid                    => open,
      expansionram_current_cache_line_next_toggle => '0',
      viciv_addr                                  => (others => '0'),
      viciv_request_toggle                        => '0',
      viciv_data_out                              => open,
      viciv_data_strobe                           => open,
      hr_d                                        => hr_d,
      hr_rwds                                     => hr_rwds,
      hr_reset                                    => hr_reset,
      hr_clk_n                                    => hr_clk_n,
      hr_clk_p                                    => hr_clk_p,
      hr2_d                                       => (others => 'Z'),
      hr2_rwds                                    => 'Z',
      hr2_reset                                   => open,
      hr2_clk_n                                   => open,
      hr2_clk_p                                   => open,
      hr_cs0                                      => hr_cs0,
      hr_cs1                                      => hr_cs1
   ); -- i_hyperram_mega65

end architecture synthesis;

