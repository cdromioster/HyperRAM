library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

entity tb is
end entity tb;

architecture simulation of tb is

   -- Testbench signals
   constant C_CLK_PERIOD    : time := 10 ns;     -- 100 MHz
   constant C_DELAY         : time := 2 ns;
   signal stop_test         : std_logic := '0';

   signal clk               : std_logic;
   signal clk_x2            : std_logic;
   signal clk_x4            : std_logic;
   signal rst               : std_logic;
   signal tb_start          : std_logic;
   signal tb_active         : std_logic;
   signal tb_error          : std_logic;

   signal avm_write         : std_logic;
   signal avm_read          : std_logic;
   signal avm_address       : std_logic_vector(31 downto 0);
   signal avm_writedata     : std_logic_vector(15 downto 0);
   signal avm_byteenable    : std_logic_vector(1 downto 0);
   signal avm_burstcount    : std_logic_vector(7 downto 0);
   signal avm_readdata      : std_logic_vector(15 downto 0);
   signal avm_readdatavalid : std_logic;
   signal avm_waitrequest   : std_logic;

   signal sys_resetn        : std_logic;
   signal sys_csn           : std_logic;
   signal sys_ck            : std_logic;
   signal sys_rwds          : std_logic;
   signal sys_dq            : unsigned(7 downto 0);

   -- HyperRAM simulation device interface
   signal hr_resetn         : std_logic;
   signal hr_csn            : std_logic;
   signal hr_ck             : std_logic;
   signal hr_rwds           : std_logic;
   signal hr_dq             : std_logic_vector(7 downto 0);

   component s27kl0642 is
      port (
         DQ7      : inout std_logic;
         DQ6      : inout std_logic;
         DQ5      : inout std_logic;
         DQ4      : inout std_logic;
         DQ3      : inout std_logic;
         DQ2      : inout std_logic;
         DQ1      : inout std_logic;
         DQ0      : inout std_logic;
         RWDS     : inout std_logic;
         CSNeg    : in    std_logic;
         CK       : in    std_logic;
         CKn      : in    std_logic;
         RESETNeg : in    std_logic
      );
   end component s27kl0642;

begin

   ---------------------------------------------------------
   -- Controller clock and reset
   ---------------------------------------------------------

   p_clk : process
   begin
      while stop_test = '0' loop
         clk <= '1';
         wait for C_CLK_PERIOD/2;
         clk <= '0';
         wait for C_CLK_PERIOD/2;
      end loop;
      wait;
   end process p_clk;

   p_clk_x2 : process
   begin
      wait for 1 ps;
      while stop_test = '0' loop
         clk_x2 <= '1';
         wait for C_CLK_PERIOD/4;
         clk_x2 <= '0';
         wait for C_CLK_PERIOD/4;
      end loop;
      wait;
   end process p_clk_x2;

   p_clk_x4 : process
   begin
      wait for 2 ps;
      while stop_test = '0' loop
         clk_x4 <= '1';
         wait for C_CLK_PERIOD/8;
         clk_x4 <= '0';
         wait for C_CLK_PERIOD/8;
      end loop;
      wait;
   end process p_clk_x4;

   p_rst : process
   begin
      rst <= '1';
      wait for 10*C_CLK_PERIOD;
      wait until clk = '1';
      rst <= '0';
      wait;
   end process p_rst;

   p_tb_start : process (clk)
   begin
      if rising_edge(clk) then
         if tb_active = '1' then
            tb_start <= '0';
         end if;
         if rst = '1' then
            tb_start <= '1';
         end if;
      end if;
   end process p_tb_start;


   --------------------------------------------------------
   -- Instantiate trafic generator
   --------------------------------------------------------

   i_trafic_gen : entity work.trafic_gen
      generic map (
         G_ADDRESS_SIZE => 3
      )
      port map (
         clk_i               => clk,
         rst_i               => rst,
         avm_write_o         => avm_write,
         avm_read_o          => avm_read,
         avm_address_o       => avm_address,
         avm_writedata_o     => avm_writedata,
         avm_byteenable_o    => avm_byteenable,
         avm_burstcount_o    => avm_burstcount,
         avm_readdata_i      => avm_readdata,
         avm_readdatavalid_i => avm_readdatavalid,
         avm_waitrequest_i   => avm_waitrequest,
         start_i             => tb_start,
         active_o            => tb_active,
         error_o             => tb_error,
         address_o           => open,
         data_exp_o          => open,
         data_read_o         => open
      ); -- i_trafic_gen


   --------------------------------------------------------
   -- Instantiate HyperRAM controller
   --------------------------------------------------------

   i_hyperram_wrapper : entity work.hyperram_wrapper
      port map (
         clk_i               => clk,
         clk_x2_i            => clk_x2,
         clk_x4_i            => clk_x4,
         rst_i               => rst,
         avm_write_i         => avm_write,
         avm_read_i          => avm_read,
         avm_address_i       => avm_address,
         avm_writedata_i     => avm_writedata,
         avm_byteenable_i    => avm_byteenable,
         avm_burstcount_i    => avm_burstcount,
         avm_readdata_o      => avm_readdata,
         avm_readdatavalid_o => avm_readdatavalid,
         avm_waitrequest_o   => avm_waitrequest,
         hr_resetn_o         => sys_resetn,
         hr_csn_o            => sys_csn,
         hr_ck_o             => sys_ck,
         hr_rwds_io          => sys_rwds,
         hr_dq_io            => sys_dq
      ); -- i_hyperram


   ---------------------------------------------------------
   -- Connect controller to device
   ---------------------------------------------------------

   hr_resetn <= sys_resetn;
   hr_csn    <= sys_csn;
   hr_ck     <= sys_ck;

   i_wiredelay2_rwds : entity work.wiredelay2
      generic map (
         G_DELAY => C_DELAY
      )
      port map (
         A => sys_rwds,
         B => hr_rwds
      );

   gen_dq_delay : for i in 0 to 7 generate
   i_wiredelay2_rwds : entity work.wiredelay2
      generic map (
         G_DELAY => C_DELAY
      )
      port map (
         A => sys_dq(i),
         B => hr_dq(i)
      );
   end generate gen_dq_delay;


   ---------------------------------------------------------
   -- Instantiate HyperRAM simulation model
   ---------------------------------------------------------

   i_s27kl0642 : s27kl0642
      port map (
         DQ7      => hr_dq(7),
         DQ6      => hr_dq(6),
         DQ5      => hr_dq(5),
         DQ4      => hr_dq(4),
         DQ3      => hr_dq(3),
         DQ2      => hr_dq(2),
         DQ1      => hr_dq(1),
         DQ0      => hr_dq(0),
         RWDS     => hr_rwds,
         CSNeg    => hr_csn,
         CK       => hr_ck,
         CKn      => not hr_ck,
         RESETNeg => hr_resetn
      ); -- i_s27kl0642

end architecture simulation;

