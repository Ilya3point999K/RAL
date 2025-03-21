#include <stdio.h>
#include "SDL.h"
#include "ral.h"

RAL_TEXTURE load_texture_from_tga(const char *filename) {
    FILE *fp = fopen(filename, "rb");

    uint8_t header[18];
    fread(header, 1, 18, fp);

    RAL_TEXTURE result;
    result.width = header[12] | (header[13] << 8);
    result.height = header[14] | (header[15] << 8);

    int length = result.width * result.height * sizeof(uint32_t);
    result.pixels = malloc(length);
    fread(result.pixels, 1, length, fp);
    fclose(fp);

    return result;
}

int main(int argument_count, char ** arguments) {
    int width = 320;
    int height = 240;

    setbuf(stdout, NULL);
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window * window = SDL_CreateWindow("", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 800, 600, 0);
    SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
    SDL_Texture * texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, width, height);
    SDL_SetWindowResizable(window, SDL_TRUE);
    SDL_RenderSetLogicalSize(renderer, width, height);
    RAL_CONTEXT context = {0};
    context.far = RAL_ONE * 20;
    context.near = RAL_ONE / 10;
    uint32_t * pixels = malloc(width * height * sizeof(pixels[0]));
    uint8_t * objp = malloc(width * height * sizeof(objp[0]));
    RAL_F * depth = malloc(width * height * sizeof(depth[0]));
    RAL_INIT(&context, pixels, depth, objp, width, height, RAL_I2F(60));
    
    //Thanks drummyfish for this texture
    RAL_TEXTURE tex = load_texture_from_tga("texture.tga");

    RAL_F player_x = 0;
    RAL_F player_z = -RAL_HALF * 5;
    RAL_F player_height = 0;
    RAL_F player_yaw = 0;
    RAL_F player_pitch = 0;
    RAL_F player_forward_speed = 0;
    RAL_F player_strafe_speed = 0;
    RAL_F mouse_sensitivity = RAL_ONE / 100;
    int up = 0, down = 0, left = 0, right = 0, crouch = 0, lift = 0;
    SDL_SetRelativeMouseMode(1);
    
    while (1) {
      SDL_Event event;
      while (SDL_PollEvent(&event)) {
          if (event.type == SDL_QUIT) {
            exit(0);
          } else if (event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) {
            int sc = event.key.keysym.scancode;
            if (sc == SDL_SCANCODE_UP || sc == SDL_SCANCODE_W) {
              up = event.key.state;
            } else if (sc == SDL_SCANCODE_DOWN || sc == SDL_SCANCODE_S) {
              down = event.key.state;
            } else if (sc == SDL_SCANCODE_LEFT || sc == SDL_SCANCODE_A) {
              left = event.key.state;
            } else if (sc == SDL_SCANCODE_RIGHT || sc == SDL_SCANCODE_D) {
              right = event.key.state;
            } else if (sc == SDL_SCANCODE_LSHIFT || sc == SDL_SCANCODE_RSHIFT || sc == SDL_SCANCODE_C) {
              crouch = event.key.state;
            } else if (sc == SDL_SCANCODE_ESCAPE) {
              exit(0);
            } else if (sc == SDL_SCANCODE_SPACE) {
              lift = event.key.state;
            }
          } else if (event.type == SDL_MOUSEMOTION) {
            player_yaw -= event.motion.xrel * mouse_sensitivity;
            player_pitch += event.motion.yrel * mouse_sensitivity;
            if (player_pitch < -(RAL_ONE + (RAL_ONE / 3))) player_pitch = -(RAL_ONE + (RAL_ONE / 3));
            if (player_pitch >  RAL_ONE + (RAL_ONE / 3)) player_pitch =  RAL_ONE + (RAL_ONE / 3);
        }
      }

      RAL_CLEAR(&context);

      // A basic version of standard (mouse-look) FPS controls.
      if (up)    player_forward_speed =  RAL_ONE / 100;
      if (down)  player_forward_speed = -RAL_ONE / 100;
      if (left)  player_strafe_speed =  RAL_ONE / 100;
      if (right) player_strafe_speed = -RAL_ONE / 100;
      player_height +=  (RAL_ONE / 100) * lift + (-RAL_ONE / 100) * crouch;
      player_x -= RAL_FMUL(RAL_FCOS(player_yaw - RAL_ONE - (RAL_ONE / 2)), player_forward_speed);
      player_z -= RAL_FMUL(RAL_FSIN(player_yaw - RAL_ONE - (RAL_ONE / 2)), player_forward_speed);
      player_x -= RAL_FMUL(RAL_FCOS(player_yaw), player_strafe_speed);
      player_z -= RAL_FMUL(RAL_FSIN(player_yaw), player_strafe_speed);
      player_forward_speed = 0;
      player_strafe_speed = 0;

      RAL_SET_CAMERA(&context, player_x, player_height, player_z, player_yaw, player_pitch, 0);

      RAL_RESET(&context);
      RAL_ROTX(&context, SDL_GetTicks() * 100);
      RAL_ROTY(&context, RAL_FSIN(SDL_GetTicks() * 100));
      RAL_ROTZ(&context, SDL_GetTicks() * 100);
      RAL_TRIANGLE_TEX(&context, -RAL_HALF,-RAL_HALF,-RAL_HALF, -RAL_HALF, RAL_HALF,-RAL_HALF,  RAL_HALF, RAL_HALF,-RAL_HALF, 0,0, 0,RAL_ONE, RAL_ONE,RAL_ONE, &tex);
      RAL_TRIANGLE_TEX(&context, -RAL_HALF,-RAL_HALF,-RAL_HALF,  RAL_HALF, RAL_HALF,-RAL_HALF,  RAL_HALF,-RAL_HALF,-RAL_HALF, 0,0, RAL_ONE,RAL_ONE, RAL_ONE,0, &tex);
      RAL_TRIANGLE_TEX(&context,  RAL_HALF,-RAL_HALF,-RAL_HALF,  RAL_HALF, RAL_HALF,-RAL_HALF,  RAL_HALF, RAL_HALF, RAL_HALF, 0,0, RAL_ONE,0, RAL_ONE,RAL_ONE, &tex);
      RAL_TRIANGLE_TEX(&context,  RAL_HALF,-RAL_HALF,-RAL_HALF,  RAL_HALF, RAL_HALF, RAL_HALF,  RAL_HALF,-RAL_HALF, RAL_HALF, 0,0, RAL_ONE,RAL_ONE, 0,RAL_ONE, &tex);
      RAL_TRIANGLE_TEX(&context,  RAL_HALF,-RAL_HALF, RAL_HALF,  RAL_HALF, RAL_HALF, RAL_HALF, -RAL_HALF, RAL_HALF, RAL_HALF, 0,0, RAL_ONE,0, RAL_ONE,RAL_ONE, &tex);
      RAL_TRIANGLE_TEX(&context,  RAL_HALF,-RAL_HALF, RAL_HALF, -RAL_HALF, RAL_HALF, RAL_HALF, -RAL_HALF,-RAL_HALF, RAL_HALF, 0,0, RAL_ONE,RAL_ONE, RAL_ONE,0, &tex);
      RAL_TRIANGLE_TEX(&context, -RAL_HALF,-RAL_HALF, RAL_HALF, -RAL_HALF, RAL_HALF, RAL_HALF, -RAL_HALF, RAL_HALF,-RAL_HALF, 0,RAL_ONE, RAL_ONE,RAL_ONE, RAL_ONE,0, &tex);
      RAL_TRIANGLE_TEX(&context, -RAL_HALF,-RAL_HALF, RAL_HALF, -RAL_HALF, RAL_HALF,-RAL_HALF, -RAL_HALF,-RAL_HALF,-RAL_HALF, 0,RAL_ONE, RAL_ONE,0, 0,0, &tex);
      RAL_TRIANGLE_TEX(&context, -RAL_HALF, RAL_HALF,-RAL_HALF, -RAL_HALF, RAL_HALF, RAL_HALF,  RAL_HALF, RAL_HALF, RAL_HALF, 0,0, 0,RAL_ONE, RAL_ONE,RAL_ONE, &tex);
      RAL_TRIANGLE_TEX(&context, -RAL_HALF, RAL_HALF,-RAL_HALF,  RAL_HALF, RAL_HALF, RAL_HALF,  RAL_HALF, RAL_HALF,-RAL_HALF, 0,0, RAL_ONE,RAL_ONE, RAL_ONE,0, &tex);
      RAL_TRIANGLE_TEX(&context,  RAL_HALF,-RAL_HALF, RAL_HALF, -RAL_HALF,-RAL_HALF, RAL_HALF, -RAL_HALF,-RAL_HALF,-RAL_HALF, 0,0, RAL_ONE,0, RAL_ONE,RAL_ONE, &tex);
      RAL_TRIANGLE_TEX(&context,  RAL_HALF,-RAL_HALF, RAL_HALF, -RAL_HALF,-RAL_HALF,-RAL_HALF,  RAL_HALF,-RAL_HALF,-RAL_HALF, 0,0, RAL_ONE,RAL_ONE, RAL_ONE,0, &tex);

      SDL_Delay(1);
      SDL_RenderClear(renderer);
      SDL_UpdateTexture(texture, NULL, pixels, width * sizeof(uint32_t));
      SDL_RenderCopy(renderer, texture, NULL, NULL);
      SDL_RenderPresent(renderer);
  }
}
