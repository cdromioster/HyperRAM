library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

library work;
use work.video_modes_pkg.all;

entity digits is
   generic (
      G_FONT_FILE   : string;
      G_DIGITS_SIZE : integer;
      G_VIDEO_MODE  : video_modes_t
   );
   port (
      clk_i    : in  std_logic;
      digits_i : in  std_logic_vector(G_DIGITS_SIZE-1 downto 0);
      pix_x_i  : in  std_logic_vector(G_VIDEO_MODE.PIX_SIZE-1 downto 0);
      pix_y_i  : in  std_logic_vector(G_VIDEO_MODE.PIX_SIZE-1 downto 0);
      pixel_o  : out std_logic_vector(7 downto 0)
   );
end entity digits;

architecture synthesis of digits is

   -- Define positioning of first digit
   constant DIGITS_CHAR_X : integer := 15;
   constant DIGITS_CHAR_Y : integer := 10;

   -- A single character bitmap is defined by 8x8 = 64 bits.
   subtype bitmap_t is std_logic_vector(63 downto 0);

   -- Define colours
   constant PIXEL_BLACK : std_logic_vector(7 downto 0) := B"000_000_00";
   constant PIXEL_DARK  : std_logic_vector(7 downto 0) := B"001_001_01";
   constant PIXEL_GREY  : std_logic_vector(7 downto 0) := B"010_010_01";
   constant PIXEL_LIGHT : std_logic_vector(7 downto 0) := B"100_100_10";
   constant PIXEL_WHITE : std_logic_vector(7 downto 0) := B"111_111_11";

   -- Character coordinates
   signal char_col     : integer range 0 to G_VIDEO_MODE.H_MAX/16-1;
   signal char_row     : integer range 0 to G_VIDEO_MODE.V_MAX/16-1;

   -- Value of nibble at current position
   signal nibble_index : integer range 0 to G_DIGITS_SIZE/4-1;
   signal nibble       : std_logic_vector(3 downto 0);

   -- Bitmap of digit at current position
   signal char         : std_logic_vector(7 downto 0);
   signal bitmap       : bitmap_t;

   -- Pixel at current position
   signal pix_col      : integer range 0 to 7;
   signal pix_row      : integer range 0 to 7;
   signal bitmap_index : integer range 0 to 63;
   signal pix          : std_logic;

begin

   --------------------------------------------------
   -- Calculate character coordinates, within 40x30
   --------------------------------------------------

   char_col <= to_integer(pix_x_i(10 downto 5));
   char_row <= to_integer(pix_y_i(10 downto 5));


   --------------------------------------------------
   -- Calculate value of nibble at current position
   --------------------------------------------------

   nibble_index <= (G_DIGITS_SIZE/4-1 - (char_col - DIGITS_CHAR_X)) mod (G_DIGITS_SIZE/4);
   nibble       <= digits_i(4*nibble_index+3 downto 4*nibble_index);


   --------------------------------------------------
   -- Calculate character to display at current position
   --------------------------------------------------

   char <= nibble + X"30" when nibble < 10 else
           nibble + X"41" - 10;


   --------------------------------------------------
   -- Calculate bitmap (64 bits) of digit at current position
   --------------------------------------------------

   i_font : entity work.font
      generic map (
         G_FONT_FILE => G_FONT_FILE
      )
      port map (
         char_i   => char,
         bitmap_o => bitmap
      ); -- i_font


   --------------------------------------------------
   -- Calculate pixel at current position ('0' or '1')
   --------------------------------------------------

   pix_col       <= to_integer(pix_x_i(4 downto 2));
   pix_row       <= 7 - to_integer(pix_y_i(4 downto 2));
   bitmap_index  <= pix_row*8 + pix_col;
   pix           <= bitmap(bitmap_index);


   --------------------------------------------------
   -- Generate pixel colour
   --------------------------------------------------

   p_pixel : process (clk_i)
   begin
      if rising_edge(clk_i) then

         -- Set the default screen background colour
         pixel_o <= PIXEL_GREY;

         -- Are we within the borders of the text?
         if char_row = DIGITS_CHAR_Y and
            char_col >= DIGITS_CHAR_X and char_col < DIGITS_CHAR_X+G_DIGITS_SIZE/4 then

            if pix = '1' then
               pixel_o <= PIXEL_LIGHT;   -- Text foreground colour.
            else
               pixel_o <= PIXEL_DARK;    -- Text background colour.
            end if;
         end if;

         -- Make sure colour is black outside visible screen
         if pix_x_i >= G_VIDEO_MODE.H_PIXELS or pix_y_i >= G_VIDEO_MODE.V_PIXELS then
            pixel_o <= PIXEL_BLACK;
         end if;

      end if;
   end process p_pixel;

end architecture synthesis;

