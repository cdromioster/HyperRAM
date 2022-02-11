library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mem is
   generic (
      G_ADDRESS_SIZE : integer; -- Number of bits
      G_DATA_SIZE    : integer  -- Number of bits
   );
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;
      avm_write_i         : in  std_logic;
      avm_read_i          : in  std_logic;
      avm_address_i       : in  std_logic_vector(G_ADDRESS_SIZE-1 downto 0);
      avm_writedata_i     : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_byteenable_i    : in  std_logic_vector(G_DATA_SIZE/8-1 downto 0);
      avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      avm_readdata_o      : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      avm_readdatavalid_o : out std_logic;
      avm_waitrequest_o   : out std_logic
   );
end entity mem;

architecture simulation of mem is

   -- This defines a type containing an array of bytes
   type mem_t is array (0 to 2**G_ADDRESS_SIZE-1) of std_logic_vector(G_DATA_SIZE-1 downto 0);

   signal mem : mem_t;

   signal burstcount  : std_logic_vector(7 downto 0);
   signal address     : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);

   signal mem_burstcount : std_logic_vector(7 downto 0);
   signal mem_address    : std_logic_vector(G_ADDRESS_SIZE-1 downto 0);

begin

   mem_address    <= avm_address_i    when burstcount = X"00" else address;
   mem_burstcount <= avm_burstcount_i when burstcount = X"00" else burstcount;

   avm_waitrequest_o <= '0';

   p_mem : process (clk_i)
   begin
      if rising_edge(clk_i) then
         avm_readdatavalid_o <= '0';

         address <= std_logic_vector(unsigned(mem_address) + 1);

         if avm_write_i = '1' and avm_waitrequest_o = '0' then
            burstcount <= std_logic_vector(unsigned(mem_burstcount) - 1);

            report "Writing 0x" & to_hstring(avm_writedata_i) & " to 0x" & to_hstring(mem_address) &
                   " with burstcount " & to_hstring(burstcount);
            for b in 0 to G_DATA_SIZE/8-1 loop
               if avm_byteenable_i(b) = '1' then
                  mem(to_integer(unsigned(mem_address)))(8*b+7 downto 8*b) <= avm_writedata_i(8*b+7 downto 8*b);
               end if;
            end loop;
         end if;

         if avm_read_i = '1' and avm_waitrequest_o = '0' then
            avm_readdata_o <= mem(to_integer(unsigned(mem_address)));
            avm_readdatavalid_o <= '1';
         end if;

         if rst_i = '1' then
            burstcount <= (others => '0');
         end if;
      end if;
   end process p_mem;

end architecture simulation;

