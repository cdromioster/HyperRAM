library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.video_modes_pkg.all;

entity video is
   generic (
      G_FONT_FILE   : string;
      G_DIGITS_SIZE : integer;
      G_VIDEO_MODE  : video_modes_t
   );
   port (
      clk_i         : in  std_logic;
      rst_i         : in  std_logic;
      digits_i      : in  std_logic_vector(G_DIGITS_SIZE-1 downto 0);
      video_vs_o    : out std_logic;
      video_hs_o    : out std_logic;
      video_de_o    : out std_logic;
      video_red_o   : out std_logic_vector(7 downto 0);
      video_green_o : out std_logic_vector(7 downto 0);
      video_blue_o  : out std_logic_vector(7 downto 0)
   );
end entity video;

architecture synthesis of video is

   signal pixel_x   : std_logic_vector(G_VIDEO_MODE.PIX_SIZE-1 downto 0);
   signal pixel_y   : std_logic_vector(G_VIDEO_MODE.PIX_SIZE-1 downto 0);
   signal vs        : std_logic;
   signal hs        : std_logic;
   signal de        : std_logic;
   signal pixel     : std_logic_vector(7 downto 0);

begin

   i_video_sync : entity work.video_sync
      generic map (
         G_VIDEO_MODE => G_VIDEO_MODE
      )
      port map (
         clk_i     => clk_i,
         rst_i     => rst_i,
         vs_o      => vs,
         hs_o      => hs,
         de_o      => de,
         pixel_x_o => pixel_x,
         pixel_y_o => pixel_y
      );

   i_digits : entity work.digits
      generic map (
         G_FONT_FILE   => G_FONT_FILE,
         G_DIGITS_SIZE => G_DIGITS_SIZE,
         G_VIDEO_MODE  => G_VIDEO_MODE
      )
      port map (
         clk_i    => clk_i,
         digits_i => digits_i,
         pix_x_i  => pixel_x,
         pix_y_i  => pixel_y,
         pixel_o  => pixel
      ); -- i_digits

   video_red_o   <= pixel;
   video_green_o <= pixel;
   video_blue_o  <= pixel;

   p_pixel : process (clk_i)
   begin
      if rising_edge(clk_i) then
         video_vs_o    <= vs;
         video_hs_o    <= hs;
         video_de_o    <= de;
      end if;
   end process p_pixel;

end architecture synthesis;

