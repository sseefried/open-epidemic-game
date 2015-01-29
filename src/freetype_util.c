#include <stdio.h>
#include <ft2build.h>
#include <cairo.h>
#include <cairo-ft.h>
#include FT_FREETYPE_H

void exitOnError(FT_Error error) {
  if (error > 0) {
    printf("FreeType 2 error: %d\n", error);
    exit(1);
  }
}

cairo_font_face_t *load_font_face() {
  FT_Library lib;
  FT_Face face;
  FT_Error error;
  cairo_font_face_t *cr_face;
  char *path = "/Users/sseefried/code/games/epidemic-game/font.ttf";

  error = FT_Init_FreeType(&lib);
  exitOnError(error);
  error = FT_New_Face(lib, path, 0, &face);
  exitOnError(error);

  cr_face = cairo_ft_font_face_create_for_ft_face(face, 0);


  return cr_face;
}


