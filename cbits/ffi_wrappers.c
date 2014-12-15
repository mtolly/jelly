#include "SDL.h"
#include "SDL_ttf.h"

SDL_Surface *hs_TTF_RenderUTF8_Blended
    ( TTF_Font *font
    , const char *text
    , Uint8 r, Uint8 g, Uint8 b, Uint8 a
    ) {
  SDL_Color color = {r, g, b, a};
  return TTF_RenderUTF8_Blended(font, text, color);
}
