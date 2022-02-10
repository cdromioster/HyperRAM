library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity trafic_gen is
   generic (
      G_ADDRESS_SIZE : integer; -- Number of bits
      G_DATA_SIZE    : integer  -- Number of bits
   );
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;
      avm_write_o         : out std_logic;
      avm_read_o          : out std_logic;
      avm_address_o       : out std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      avm_writedata_o     : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_byteenable_o    : out std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      avm_burstcount_o    : out std_logic_vector(7 downto 0);
      avm_readdata_i      : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_readdatavalid_i : in  std_logic;
      avm_waitrequest_i   : in  std_logic
   );
end entity trafic_gen;

architecture simulation of trafic_gen is

   constant C_DATA_INIT : std_logic_vector(63 downto 0) := X"CAFEBABEDEADBEEF";

   signal address : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
   signal data    : std_logic_vector(63 downto 0);

   type state_t is (
      INIT_ST,
      WRITING_ST,
      READING_ST,
      VERIFYING_ST,
      STOPPED_ST
   );

   signal state : state_t := INIT_ST;

begin

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if avm_waitrequest_i = '0' then
            avm_write_o <= '0';
            avm_read_o  <= '0';
         end if;

         case state is
            when INIT_ST =>
               address  <= (others => '0');
               data     <= C_DATA_INIT;
               state    <= WRITING_ST;

            when WRITING_ST =>
               avm_write_o      <= '1';
               avm_read_o       <= '0';
               avm_address_o    <= (others => '0');
               avm_address_o(G_ADDRESS_SIZE-1 downto 0) <= address;
               avm_writedata_o  <= data(G_DATA_SIZE-1 downto 0);
               avm_byteenable_o <= (others => '1');
               avm_burstcount_o <= X"01";

               if avm_write_o = '1' and avm_waitrequest_i = '0' then
                  -- Increment address linearly
                  address <= std_logic_vector(unsigned(address) + 1);

                  -- The pseudo-random data is generated using a 64-bit maximal-period Galois LFSR,
                  -- see http://users.ece.cmu.edu/~koopman/lfsr/64.txt
                  if data(63) = '1' then
                     data <= (data(62 downto 0) & "0") xor X"000000000000001B";
                  else
                     data <= (data(62 downto 0) & "0");
                  end if;

                  if signed(address) = -1 then
                     data  <= C_DATA_INIT;
                     state <= READING_ST;
                  end if;
               end if;

            when READING_ST =>
               avm_write_o      <= '0';
               avm_read_o       <= '1';
               avm_address_o    <= (others => '0');
               avm_address_o(G_ADDRESS_SIZE-1 downto 0) <= address;
               avm_burstcount_o <= X"01";

               if avm_read_o = '1' and avm_waitrequest_i = '0' then
                  avm_read_o  <= '0';
                  state       <= VERIFYING_ST;
               end if;

            when VERIFYING_ST =>
               if avm_readdatavalid_i = '1' then

                  address <= std_logic_vector(unsigned(address) + 1);
                  if data(63) = '1' then
                     data <= (data(62 downto 0) & "0") xor X"000000000000001B";
                  else
                     data <= (data(62 downto 0) & "0");
                  end if;

                  if avm_readdata_i /= data(G_DATA_SIZE-1 downto 0) then
                     report "ERROR: Expected " & to_hstring(data(G_DATA_SIZE-1 downto 0)) & ", read " & to_hstring(avm_readdata_i);
--                     state    <= STOPPED_ST;
                  elsif signed(address) = -1 then
                     report "Test stopped";
                     data     <= C_DATA_INIT;
                     state    <= STOPPED_ST;
                  else
                     state <= READING_ST;
                  end if;
               end if;

            when STOPPED_ST =>
               null;
         end case;

         if rst_i = '1' then
            avm_write_o <= '0';
            avm_read_o  <= '0';
            state       <= INIT_ST;
         end if;
      end if;
   end process p_fsm;

end architecture simulation;

