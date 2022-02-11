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
   constant C_WORD_COUNT : integer := G_DATA_SIZE/16; -- 16 bits in each word

   signal address     : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
   signal data        : std_logic_vector(63 downto 0);
   signal new_address : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
   signal new_data    : std_logic_vector(63 downto 0);

   type state_t is (
      INIT_ST,
      WRITING_ST,
      READING_ST,
      VERIFYING_ST,
      STOPPED_ST
   );

   signal state : state_t := INIT_ST;

begin

   -- The pseudo-random data is generated using a 64-bit maximal-period Galois LFSR,
   -- see http://users.ece.cmu.edu/~koopman/lfsr/64.txt
   new_data <= (data(62 downto 0) & "0") xor x"000000000000001b" when data(63) = '1' else
               (data(62 downto 0) & "0");
   new_address <= std_logic_vector(unsigned(address) + C_WORD_COUNT);

   p_fsm : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if avm_waitrequest_i = '0' then
            avm_write_o <= '0';
            avm_read_o  <= '0';
         end if;

         case state is
            when INIT_ST =>
               address          <= (others => '0');
               data             <= C_DATA_INIT;
               avm_write_o      <= '1';
               avm_read_o       <= '0';
               avm_address_o    <= (others => '0');
               avm_writedata_o  <= C_DATA_INIT(G_DATA_SIZE-1 downto 0);
               avm_byteenable_o <= (others => '1');
               avm_burstcount_o <= X"01";
               state            <= WRITING_ST;

            when WRITING_ST =>
               if avm_waitrequest_i = '0' then
                  avm_write_o      <= '1';
                  avm_read_o       <= '0';
                  avm_address_o    <= (others => '0');
                  avm_address_o(G_ADDRESS_SIZE-1 downto 0) <= new_address;
                  avm_writedata_o  <= new_data(G_DATA_SIZE-1 downto 0);
                  avm_byteenable_o <= (others => '1');
                  avm_burstcount_o <= X"01";

                  address <= new_address;
                  data    <= new_data;

                  if signed(address) = -C_WORD_COUNT then
                     data          <= C_DATA_INIT;
                     avm_write_o   <= '0';
                     avm_address_o <= (others => '0');
                     avm_read_o    <= '1';
                     state         <= READING_ST;
                  end if;
               end if;

            when READING_ST =>
               if avm_waitrequest_i = '0' then
                  state <= VERIFYING_ST;
               end if;

            when VERIFYING_ST =>
               if avm_readdatavalid_i = '1' then

                  if avm_readdata_i /= data(G_DATA_SIZE-1 downto 0) then
                     report "ERROR: Expected " & to_hstring(data(G_DATA_SIZE-1 downto 0)) & ", read " & to_hstring(avm_readdata_i);
                     state  <= STOPPED_ST;
                  elsif signed(address) = -C_WORD_COUNT then
                     report "Test stopped";
                     data  <= C_DATA_INIT;
                     state <= STOPPED_ST;
                  else
                     avm_address_o <= new_address;
                     address       <= new_address;
                     data          <= new_data;
                     avm_read_o    <= '1';
                     state         <= READING_ST;
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

