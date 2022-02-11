library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_avm_decrease is
end entity tb_avm_decrease;

architecture simulation of tb_avm_decrease is

   constant C_SLAVE_DATA_SIZE  : integer := 32;
   constant C_MASTER_DATA_SIZE : integer := 16;
   constant C_ADDRESS_SIZE     : integer := 3;

   signal clk                 : std_logic;
   signal rst                 : std_logic;
   signal s_avm_write         : std_logic;
   signal s_avm_read          : std_logic;
   signal s_avm_address       : std_logic_vector(C_ADDRESS_SIZE-1 downto 0);
   signal s_avm_writedata     : std_logic_vector(C_SLAVE_DATA_SIZE-1 downto 0);
   signal s_avm_byteenable    : std_logic_vector(C_SLAVE_DATA_SIZE/8-1 downto 0);
   signal s_avm_burstcount    : std_logic_vector(7 downto 0);
   signal s_avm_readdata      : std_logic_vector(C_SLAVE_DATA_SIZE-1 downto 0);
   signal s_avm_readdatavalid : std_logic;
   signal s_avm_waitrequest   : std_logic;

   signal m_avm_write         : std_logic;
   signal m_avm_read          : std_logic;
   signal m_avm_address       : std_logic_vector(C_ADDRESS_SIZE-1 downto 0);
   signal m_avm_writedata     : std_logic_vector(C_MASTER_DATA_SIZE-1 downto 0);
   signal m_avm_byteenable    : std_logic_vector(C_MASTER_DATA_SIZE/8-1 downto 0);
   signal m_avm_burstcount    : std_logic_vector(7 downto 0);
   signal m_avm_readdata      : std_logic_vector(C_MASTER_DATA_SIZE-1 downto 0);
   signal m_avm_readdatavalid : std_logic;
   signal m_avm_waitrequest   : std_logic;

   signal p_avm_write         : std_logic;
   signal p_avm_read          : std_logic;
   signal p_avm_address       : std_logic_vector(C_ADDRESS_SIZE-1 downto 0);
   signal p_avm_writedata     : std_logic_vector(C_MASTER_DATA_SIZE-1 downto 0);
   signal p_avm_byteenable    : std_logic_vector(C_MASTER_DATA_SIZE/8-1 downto 0);
   signal p_avm_burstcount    : std_logic_vector(7 downto 0);
   signal p_avm_readdata      : std_logic_vector(C_MASTER_DATA_SIZE-1 downto 0);
   signal p_avm_readdatavalid : std_logic;
   signal p_avm_waitrequest   : std_logic;

   signal stop_test : std_logic := '0';
   constant C_CLK_PERIOD : time := 10 ns;

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

   p_rst : process
   begin
      rst <= '1';
      wait for 10*C_CLK_PERIOD;
      wait until clk = '1';
      rst <= '0';
      wait;
   end process p_rst;


   ---------------------------------------------------------
   -- Instantiate Master
   ---------------------------------------------------------

   i_trafic_gen : entity work.trafic_gen
      generic map (
         G_ADDRESS_SIZE => C_ADDRESS_SIZE,
         G_DATA_SIZE    => C_SLAVE_DATA_SIZE
      )
      port map (
         clk_i               => clk,
         rst_i               => rst,
         avm_write_o         => s_avm_write,
         avm_read_o          => s_avm_read,
         avm_address_o       => s_avm_address,
         avm_writedata_o     => s_avm_writedata,
         avm_byteenable_o    => s_avm_byteenable,
         avm_burstcount_o    => s_avm_burstcount,
         avm_readdata_i      => s_avm_readdata,
         avm_readdatavalid_i => s_avm_readdatavalid,
         avm_waitrequest_i   => s_avm_waitrequest
      ); -- i_trafic_gen


   ---------------------------------------------------------
   -- Instantiate DUT
   ---------------------------------------------------------

   i_avm_decrease : entity work.avm_decrease
      generic map (
         G_ADDRESS_SIZE     => C_ADDRESS_SIZE,
         G_SLAVE_DATA_SIZE  => C_SLAVE_DATA_SIZE,
         G_MASTER_DATA_SIZE => C_MASTER_DATA_SIZE
      )
      port map (
         clk_i                  => clk,
         rst_i                  => rst,
         s_avm_write_i          => s_avm_write,
         s_avm_read_i           => s_avm_read,
         s_avm_address_i        => s_avm_address,
         s_avm_writedata_i      => s_avm_writedata,
         s_avm_byteenable_i     => s_avm_byteenable,
         s_avm_burstcount_i     => s_avm_burstcount,
         s_avm_readdata_o       => s_avm_readdata,
         s_avm_readdatavalid_o  => s_avm_readdatavalid,
         s_avm_waitrequest_o    => s_avm_waitrequest,
         m_avm_write_o          => m_avm_write,
         m_avm_read_o           => m_avm_read,
         m_avm_address_o        => m_avm_address,
         m_avm_writedata_o      => m_avm_writedata,
         m_avm_byteenable_o     => m_avm_byteenable,
         m_avm_burstcount_o     => m_avm_burstcount,
         m_avm_readdata_i       => m_avm_readdata,
         m_avm_readdatavalid_i  => m_avm_readdatavalid,
         m_avm_waitrequest_i    => m_avm_waitrequest
      ); -- i_avm_decrease

--   m_avm_write         <= s_avm_write;
--   m_avm_read          <= s_avm_read;
--   m_avm_address       <= s_avm_address;
--   m_avm_writedata     <= s_avm_writedata;
--   m_avm_byteenable    <= s_avm_byteenable;
--   m_avm_burstcount    <= s_avm_burstcount;
--   s_avm_readdata      <= m_avm_readdata;
--   s_avm_readdatavalid <= m_avm_readdatavalid;
--   s_avm_waitrequest   <= m_avm_waitrequest;


   ---------------------------------------------------------
   -- Generate pauses in slave reception
   ---------------------------------------------------------

   i_avm_pause : entity work.avm_pause
      generic map (
         G_PAUSE        => 2,
         G_ADDRESS_SIZE => C_ADDRESS_SIZE,
         G_DATA_SIZE    => C_MASTER_DATA_SIZE
      )
      port map (
         clk_i                 => clk,
         rst_i                 => rst,
         s_avm_write_i         => m_avm_write,
         s_avm_read_i          => m_avm_read,
         s_avm_address_i       => m_avm_address,
         s_avm_writedata_i     => m_avm_writedata,
         s_avm_byteenable_i    => m_avm_byteenable,
         s_avm_burstcount_i    => m_avm_burstcount,
         s_avm_readdata_o      => m_avm_readdata,
         s_avm_readdatavalid_o => m_avm_readdatavalid,
         s_avm_waitrequest_o   => m_avm_waitrequest,
         m_avm_write_o         => p_avm_write,
         m_avm_read_o          => p_avm_read,
         m_avm_address_o       => p_avm_address,
         m_avm_writedata_o     => p_avm_writedata,
         m_avm_byteenable_o    => p_avm_byteenable,
         m_avm_burstcount_o    => p_avm_burstcount,
         m_avm_readdata_i      => p_avm_readdata,
         m_avm_readdatavalid_i => p_avm_readdatavalid,
         m_avm_waitrequest_i   => p_avm_waitrequest
      ); -- i_avm_pause

--   p_avm_write          <= m_avm_write;
--   p_avm_read           <= m_avm_read;
--   p_avm_address        <= m_avm_address;
--   p_avm_writedata      <= m_avm_writedata;
--   p_avm_byteenable     <= m_avm_byteenable;
--   p_avm_burstcount     <= m_avm_burstcount;
--   m_avm_readdata       <= p_avm_readdata;
--   m_avm_readdatavalid  <= p_avm_readdatavalid;
--   m_avm_waitrequest    <= p_avm_waitrequest;


   ---------------------------------------------------------
   -- Instantiate Slave
   ---------------------------------------------------------

   i_mem : entity work.mem
      generic map (
         G_ADDRESS_SIZE => C_ADDRESS_SIZE,
         G_DATA_SIZE    => C_MASTER_DATA_SIZE
      )
      port map (
         clk_i               => clk,
         rst_i               => rst,
         avm_write_i         => p_avm_write,
         avm_read_i          => p_avm_read,
         avm_address_i       => p_avm_address,
         avm_writedata_i     => p_avm_writedata,
         avm_byteenable_i    => p_avm_byteenable,
         avm_burstcount_i    => p_avm_burstcount,
         avm_readdata_o      => p_avm_readdata,
         avm_readdatavalid_o => p_avm_readdatavalid,
         avm_waitrequest_o   => p_avm_waitrequest
      ); -- i_mem

end architecture simulation;

