library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

-- This is the HyperRAM I/O connections

entity hyperram_io is
   port (
      clk_i               : in  std_logic;
      clk_x2_i            : in  std_logic; -- Double frequency.
      clk_x2_del_i        : in  std_logic; -- Double frequency, phase shifted
      clk_x4_i            : in  std_logic; -- Quadruple frequency.
      rst_i               : in  std_logic;

      -- Connect to HyperRAM controller
      ctrl_rstn_i         : in  std_logic;
      ctrl_ck_ddr_i       : in  std_logic_vector(1 downto 0);
      ctrl_csn_i          : in  std_logic;
      ctrl_dq_ddr_in_o    : out std_logic_vector(15 downto 0);
      ctrl_dq_ddr_out_i   : in  std_logic_vector(15 downto 0);
      ctrl_dq_oe_i        : in  std_logic;
      ctrl_dq_ie_o        : out std_logic;
      ctrl_rwds_ddr_out_i : in  std_logic_vector(1 downto 0);
      ctrl_rwds_oe_i      : in  std_logic;

      -- Connect to HyperRAM device
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
end entity hyperram_io;

architecture synthesis of hyperram_io is

   -- Output generation
   signal rwds_ddr_out_x2  : std_logic_vector(1 downto 0);
   signal dq_ddr_out_x2    : std_logic_vector(15 downto 0);

   -- Debug
   signal csn_in_x2        : std_logic;
   signal rwds_in_x2       : std_logic;
   signal dq_in_x2         : std_logic_vector(7 downto 0);

   -- Input sampling
   signal hr_rwds_in_x4    : std_logic;
   signal hr_dq_ddr_in_x4  : std_logic_vector(15 downto 0);
   signal hr_dq_ie_x4      : std_logic;
   signal hr_dq_ie_hold_x4 : std_logic;

   signal ctrl_dq_ie_hold  : std_logic;

   constant C_DEBUG_MODE                    : boolean := false;
   attribute mark_debug                     : boolean;
   attribute mark_debug of rwds_in_x2       : signal is C_DEBUG_MODE;
   attribute mark_debug of dq_in_x2         : signal is C_DEBUG_MODE;
   attribute mark_debug of csn_in_x2        : signal is C_DEBUG_MODE;
   attribute mark_debug of ctrl_dq_ie_o     : signal is C_DEBUG_MODE;
   attribute mark_debug of ctrl_dq_ie_hold  : signal is C_DEBUG_MODE;
   attribute mark_debug of hr_resetn_o      : signal is C_DEBUG_MODE;
   attribute mark_debug of hr_csn_o         : signal is C_DEBUG_MODE;
   attribute mark_debug of hr_ck_o          : signal is C_DEBUG_MODE;
   attribute mark_debug of hr_rwds_out_o    : signal is C_DEBUG_MODE;
   attribute mark_debug of hr_dq_out_o      : signal is C_DEBUG_MODE;

begin

   hr_csn_o    <= ctrl_csn_i;
   hr_resetn_o <= ctrl_rstn_i;


   ------------------------------------------------
   -- Output generation
   ------------------------------------------------

   p_output_clk : process (clk_x2_del_i)
   begin
      if rising_edge(clk_x2_del_i) then
         if hr_ck_o = '0' then
            hr_ck_o <= ctrl_ck_ddr_i(1);
         else
            hr_ck_o <= '0';
         end if;
      end if;
   end process p_output_clk;

   p_output_rwds : process (clk_x2_i)
   begin
      if rising_edge(clk_x2_i) then
         rwds_ddr_out_x2 <= ctrl_rwds_ddr_out_i;
         if hr_ck_o = '0' then
            hr_rwds_out_o <= rwds_ddr_out_x2(1);
         else
            hr_rwds_out_o <= rwds_ddr_out_x2(0);
         end if;
      end if;
   end process p_output_rwds;

   p_output_dq : process (clk_x2_i)
   begin
      if rising_edge(clk_x2_i) then
         dq_ddr_out_x2 <= ctrl_dq_ddr_out_i;
         if hr_ck_o = '0' then
            hr_dq_out_o <= dq_ddr_out_x2(15 downto 8);
         else
            hr_dq_out_o <= dq_ddr_out_x2(7 downto 0);
         end if;
      end if;
   end process p_output_dq;

   p_delay : process (clk_i)
   begin
      if rising_edge(clk_i) then
         hr_dq_oe_o   <= ctrl_dq_oe_i;
         hr_rwds_oe_o <= ctrl_rwds_oe_i;
      end if;
   end process p_delay;


   ------------------------------------------------
   -- Debug
   ------------------------------------------------

   p_debug_x2 : process (clk_x2_i)
   begin
      if rising_edge(clk_x2_i) then
         csn_in_x2  <= hr_csn_o;
         rwds_in_x2 <= hr_rwds_in_i;
         dq_in_x2   <= hr_dq_in_i;
      end if;
   end process p_debug_x2;


   ------------------------------------------------
   -- Input sampling
   ------------------------------------------------

   p_input_x4 : process (clk_x4_i)
   begin
      if rising_edge(clk_x4_i) then
         hr_dq_ie_x4      <= hr_dq_ie_hold_x4;
         hr_dq_ie_hold_x4 <= '0';

         hr_rwds_in_x4 <= hr_rwds_in_i;
         if hr_rwds_in_x4 = '0' and hr_rwds_in_i = '1' then
            hr_dq_ddr_in_x4(15 downto 8) <= hr_dq_in_i;
         end if;
         if hr_rwds_in_x4 = '1' and hr_rwds_in_i = '0' then
            hr_dq_ddr_in_x4(7 downto 0) <= hr_dq_in_i;
            hr_dq_ie_x4      <= '1';
            hr_dq_ie_hold_x4 <= '1';
         end if;
      end if;
   end process p_input_x4;

   p_input_x2 : process (clk_x2_i)
   begin
      if rising_edge(clk_x2_i) then
         ctrl_dq_ie_o    <= ctrl_dq_ie_hold;
         ctrl_dq_ie_hold <= '0';

         if hr_dq_ie_x4 = '1' then
            ctrl_dq_ddr_in_o <= hr_dq_ddr_in_x4;
            ctrl_dq_ie_o    <= '1';
            ctrl_dq_ie_hold <= '1';
         end if;
      end if;
   end process p_input_x2;

end architecture synthesis;

