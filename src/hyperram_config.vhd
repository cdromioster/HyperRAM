library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity hyperram_config is
   generic (
      G_LATENCY : integer
   );
   port (
      clk_i                 : in  std_logic;
      rst_i                 : in  std_logic;

      -- Slave interface (input). Connect to client.
      s_avm_write_i         : in  std_logic;
      s_avm_read_i          : in  std_logic;
      s_avm_address_i       : in  std_logic_vector(31 downto 0);
      s_avm_writedata_i     : in  std_logic_vector(15 downto 0);
      s_avm_byteenable_i    : in  std_logic_vector(1 downto 0);
      s_avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      s_avm_readdata_o      : out std_logic_vector(15 downto 0);
      s_avm_readdatavalid_o : out std_logic;
      s_avm_waitrequest_o   : out std_logic;

      -- Master interface (output). Connect to controller.
      m_avm_write_o         : out std_logic;
      m_avm_read_o          : out std_logic;
      m_avm_address_o       : out std_logic_vector(31 downto 0);
      m_avm_writedata_o     : out std_logic_vector(15 downto 0);
      m_avm_byteenable_o    : out std_logic_vector(1 downto 0);
      m_avm_burstcount_o    : out std_logic_vector(7 downto 0);
      m_avm_readdata_i      : in  std_logic_vector(15 downto 0);
      m_avm_readdatavalid_i : in  std_logic;
      m_avm_waitrequest_i   : in  std_logic
   );
end entity hyperram_config;

architecture synthesis of hyperram_config is

   constant C_INIT_DELAY : integer := 150*100; -- 150 us @ 100 MHz.

   constant R_C0_DPD          : integer := 15;
   subtype  R_C0_DRIVE    is natural range 14 downto 12;
   subtype  R_C0_RESERCED is natural range 11 downto  8;
   subtype  R_C0_LATENCY  is natural range  7 downto  4;
   constant R_C0_FIXED        : integer :=  3;
   constant R_C0_HYBRID       : integer :=  2;
   subtype  R_C0_BURST    is natural range  1 downto  0;

   type state_t is (
      INIT_ST,
      CONFIG_ST,
      READY_ST
   );

   signal state : state_t := INIT_ST;

   signal init_counter : integer range 0 to C_INIT_DELAY;

begin

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if m_avm_waitrequest_i = '0' then
            m_avm_write_o <= '0';
            m_avm_read_o  <= '0';
         end if;
         s_avm_readdatavalid_o <= '0';

         case state is 
            when INIT_ST =>
               s_avm_waitrequest_o <= '1';
               if init_counter > 0 then
                  init_counter <= init_counter - 1;
               else
                  report "Init completed";
                  state <= CONFIG_ST;
               end if;

            when CONFIG_ST =>
               s_avm_waitrequest_o <= '1';
               -- Write to configuration register 0
               m_avm_write_o       <= '1';
               m_avm_read_o        <= '0';
               m_avm_address_o     <= (others => '0');
               m_avm_address_o(18 downto 11) <= X"01";
               m_avm_address_o(31) <= '1';
               m_avm_writedata_o(R_C0_DPD)     <= '1';    -- normal
               m_avm_writedata_o(R_C0_DRIVE)   <= "111";  -- 19 ohms
               m_avm_writedata_o(R_C0_RESERCED)<= "1111";
               m_avm_writedata_o(R_C0_LATENCY) <= std_logic_vector(to_unsigned(G_LATENCY, 4) - 5);
               m_avm_writedata_o(R_C0_FIXED)   <= '0';    -- variable
               m_avm_writedata_o(R_C0_HYBRID)  <= '1';    -- legacy
               m_avm_writedata_o(R_C0_BURST)   <= "10";   -- 16 bytes
               m_avm_byteenable_o  <= "11";
               m_avm_burstcount_o  <= X"01";

               if m_avm_write_o = '1' and m_avm_waitrequest_i = '0' then
                  state <= READY_ST;
               end if;

            when READY_ST =>
               m_avm_write_o         <= s_avm_write_i;
               m_avm_read_o          <= s_avm_read_i;
               m_avm_address_o       <= s_avm_address_i;
               m_avm_writedata_o     <= s_avm_writedata_i;
               m_avm_byteenable_o    <= s_avm_byteenable_i;
               m_avm_burstcount_o    <= s_avm_burstcount_i;
               s_avm_readdata_o      <= m_avm_readdata_i;
               s_avm_readdatavalid_o <= m_avm_readdatavalid_i;
               s_avm_waitrequest_o   <= m_avm_waitrequest_i;
         end case;

         if rst_i = '1' then
            s_avm_waitrequest_o <= '1';
            m_avm_write_o       <= '0';
            m_avm_read_o        <= '0';
            init_counter        <= C_INIT_DELAY;
            state               <= INIT_ST;
         end if;
      end if;
   end process p_fsm;

end architecture synthesis;

