/*
    A weird little demo game where you collect golden heads. It demonstrates how
    immediate-mode rendering of triangles can be used to throw together things
    with simple code. The head model is loaded from an OBJ file, and the rest of
    the terrain is generated randomly. This uses a trick where the random number
    generator seed is reset before use to recall the same set of numbers without
    having to store them.

    Use WASD or the arrow keys to move, and mouse to look.
*/

#define RND RAL_D2F(rand() / (float)RAND_MAX)

#include <time.h> // random number seed
#include <stdio.h>
#include "SDL.h"
#include "ral.h"

int main(int argument_count, char ** arguments) {
    int width = 640;
    int height = 480;

    setbuf(stdout, NULL);
    SDL_Init(SDL_INIT_VIDEO);
    SDL_Window * window = SDL_CreateWindow("", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, width, height, 0);
    SDL_Renderer * renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
    SDL_Texture * texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, width, height);
	RAL_CONTEXT context = {0};
	context.far = RAL_ONE * 100;
	context.near = RAL_ONE / 10;
    uint32_t * pixels = malloc(width * height * sizeof(pixels[0]));
    RAL_F * depth = malloc(width * height * sizeof(depth[0]));
    RAL_INIT(&context, pixels, depth, width, height, RAL_I2F(90));
    // Barebones .obj file loader.
    char * file_name = "moai.obj";
    if (argument_count == 2) file_name = arguments[1];
    int vert_count = 0;
    RAL_F * triangles = NULL;
    {
        int vi = 0, ti = 0;
        RAL_F * vertices = NULL;
        FILE * obj_file = fopen(file_name, "r");
        if (!obj_file) {
            printf("Failed to load file '%s'.\n", file_name);
            exit(1);
        }
        char line[128];
        float x, y, z;
        while (fgets(line, 128, obj_file)) {
            if (line[0] == 'v' && sscanf(line, " v %f %f %f ", &x, &y, &z)) {
                vertices = realloc(vertices, (vi+3) * sizeof(vertices[0]));
                vertices[vi++] = RAL_D2F(x);
                vertices[vi++] = RAL_D2F(y);
                vertices[vi++] = RAL_D2F(z);
            }
        }
        rewind(obj_file);
        int a, b, c;
        while (fgets(line, 128, obj_file)) {
            if (line[0] == 'f') {
                // Erase texture and normal information.
                for (int i = 0; i < 128; ++i) {
                    if (line[i] == '/') {
                        while (line[i] != ' ' && line[i] != '\n' && line[i] != '\0') {
                            line[i++] = ' ';
                        }
                    }
                }
                if (sscanf(line, " f %d %d %d ", &a, &b, &c)) {
                    a--, b--, c--;
                    triangles = realloc(triangles, (ti+9) * sizeof(triangles[0]));
                    triangles[ti++] = vertices[(a*3)+0];
                    triangles[ti++] = vertices[(a*3)+1];
                    triangles[ti++] = vertices[(a*3)+2];
                    triangles[ti++] = vertices[(b*3)+0];
                    triangles[ti++] = vertices[(b*3)+1];
                    triangles[ti++] = vertices[(b*3)+2];
                    triangles[ti++] = vertices[(c*3)+0];
                    triangles[ti++] = vertices[(c*3)+1];
                    triangles[ti++] = vertices[(c*3)+2];
                }
            }
        }
        free(vertices);
        vert_count = ti;
    }

    RAL_F player_x = -RAL_ONE;
    RAL_F player_z = -RAL_ONE * 3;
    RAL_F player_height = RAL_ONE;
    RAL_F player_yaw = RAL_ONE;
    RAL_F player_pitch = 0;
    RAL_F player_forward_speed = 0;
    RAL_F player_strafe_speed = 0;
    RAL_F mouse_sensitivity = RAL_ONE / 100;
    int up = 0, down = 0, left = 0, right = 0, crouch = 0;
    SDL_SetRelativeMouseMode(1);

    int world_size = 20 * RAL_ONE;
    RAL_F boundary = RAL_ONE * 2 + (RAL_ONE / 2);

    int seed = time(NULL);
    srand(seed);
    RAL_F heads[16];
    for (int i = 0; i < 16; ++i) {
        heads[i] = (int)((RAL_FMUL(RND, (world_size-boundary)) * 2) - (world_size-boundary));
    }
	printf("%i\n", RAL_D2F(3.1415926535));
    int heads_found = 0;
    RAL_F head_radius = RAL_ONE / 2;
    RAL_F head_confetti_y = RAL_ONE * 2;

    SDL_SetWindowTitle(window, "Find The Golden Heads");

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
                }
            } else if (event.type == SDL_MOUSEMOTION) {
                player_yaw -= event.motion.xrel * mouse_sensitivity;
                player_pitch += event.motion.yrel * mouse_sensitivity;
                if (player_pitch < -(RAL_ONE + (RAL_ONE / 3))) player_pitch = -(RAL_ONE + (RAL_ONE / 3));
                if (player_pitch >  RAL_ONE + (RAL_ONE / 3)) player_pitch =  RAL_ONE + (RAL_ONE / 3);
            }
        }

        RAL_CLEAR(&context);
        RAL_F t = SDL_GetTicks() * (RAL_ONE / 100);

        // A basic version of standard (mouse-look) FPS controls.
        if (up)    player_forward_speed =  RAL_ONE / 10;
        if (down)  player_forward_speed = -RAL_ONE / 10;
        if (left)  player_strafe_speed =  RAL_ONE / 10;
        if (right) player_strafe_speed = -RAL_ONE / 10;
        player_height += RAL_FMUL(((crouch ? RAL_ONE / 2 : RAL_ONE) - player_height), (RAL_ONE / 10));
        player_x -= RAL_FMUL(RAL_FCOS(player_yaw - RAL_ONE - (RAL_ONE / 2)), player_forward_speed);
        player_z -= RAL_FMUL(RAL_FSIN(player_yaw - RAL_ONE - (RAL_ONE / 2)), player_forward_speed);
        player_x -= RAL_FMUL(RAL_FCOS(player_yaw), player_strafe_speed);
        player_z -= RAL_FMUL(RAL_FSIN(player_yaw), player_strafe_speed);
        player_forward_speed = 0;
        player_strafe_speed = 0;

        // Collision with world edge.
        if (player_x < -(world_size-boundary)) player_x = -world_size+boundary;
        if (player_x >  (world_size-boundary)) player_x =  world_size-boundary;
        if (player_z < -(world_size-boundary)) player_z = -world_size+boundary;
        if (player_z >  (world_size-boundary)) player_z =  world_size-boundary;

        RAL_SET_CAMERA(&context, player_x, player_height, player_z, player_yaw, player_pitch, 0);

        // Draw a checkerboard floor.
        RAL_RESET(&context);
		/*
        for (int z = -RAL_F2I(world_size); z < RAL_F2I(world_size); ++z) {
            for (int x = -RAL_F2I(world_size); x < RAL_F2I(world_size); ++x) {
                uint32_t c = (x+z) & 1 ? 0x424C88 : 0xF7C396;
				RAL_F xx = RAL_I2F(x);
				RAL_F zz = RAL_I2F(z);
                RAL_TRIANGLE(&context, xx+(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), c);
                RAL_TRIANGLE(&context, xx+(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), xx+(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), c);
            }
        }
		*/
        for (int z = -20; z < 20; ++z) {
            for (int x = -20; x < 20; ++x) {
                uint32_t c = (x+z) & 1 ? 0x424C88 : 0xF7C396;
				RAL_F xx = RAL_I2F(x);
				RAL_F zz = RAL_I2F(z);
				if(z == 0 && x == 0){
				RAL_TRIANGLE(&context, xx+(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), 0xFF0000);
                RAL_TRIANGLE(&context, xx+(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), xx+(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), 0xFF0000);
				} else{
                RAL_TRIANGLE(&context, xx+(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), c);
                RAL_TRIANGLE(&context, xx+(RAL_ONE / 2), 0, zz+(RAL_ONE / 2), xx+(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), xx-(RAL_ONE / 2), 0, zz-(RAL_ONE / 2), c);
				}
            }
        }

		
        // Draw the golden heads.
        for (int h = 0; h < 16; h += 2) {
            RAL_F x = heads[h];
            RAL_F z = heads[h+1];
            // If the player is close to a head, mark it as found using NAN.
            if (RAL_ABS(player_x - x) < head_radius && RAL_ABS(player_z - z) < head_radius) {
                heads[h] = 0;
                heads[h+1] = 0;
                ++heads_found;
                char title[64];
                snprintf(title, 64, "%d / 8 heads found", heads_found);
                SDL_SetWindowTitle(window, title);
            }
            // Draw the remaining heads.
			
            if (!(heads[h] == 0)) {
                RAL_RESET(&context);
                RAL_ROTY(&context, RAL_I2F(h)+t);
                RAL_SCALE(&context, RAL_ONE ,RAL_ONE,RAL_ONE);
                RAL_TRANSLATE(&context, x, (RAL_ONE / 3) + RAL_FMUL(RAL_FSIN(RAL_I2F(h)+t),(RAL_ONE / 10)), z);
                srand(h);
                for (int i = 0; i < vert_count; i += 9) {
                    uint32_t r = 200 + (RAL_F2I(RND * 50));
                    uint32_t g = 150 + (RAL_F2I(RND * 50));
                    uint32_t b = 50  + (RAL_F2I(RND * 50));
                    RAL_TRIANGLE(&context, 
                        triangles[i + 0], triangles[i + 1], triangles[i + 2],
                        triangles[i + 3], triangles[i + 4], triangles[i + 5],
                        triangles[i + 6], triangles[i + 7], triangles[i + 8],
                        (r << 16 | g << 8 | b)
                    );
                }
            }
			
        }
		
        // Make a jagged border around the world using stretched cubes.
		
        srand(seed);
        for (int i = -RAL_F2I(world_size); i < RAL_F2I(world_size); i+=2) {
            for (int j = 0; j < 4; ++j) {
                RAL_F x = i * RAL_ONE, z = i * RAL_ONE;
                switch (j) {
                    case 0: x = -world_size; break;
                    case 1: x =  world_size; break;
                    case 2: z = -world_size; break;
                    case 3: z =  world_size; break;
                }
                RAL_RESET(&context);
                RAL_ROTY(&context, RAL_FMUL(RND, RAL_PI));
                RAL_ROTX(&context, RAL_FMUL(RND, RAL_PI));
                RAL_ROTZ(&context, RAL_FMUL(RND, RAL_PI));
                RAL_SCALE(&context, (RAL_ONE) + RND * 2, (RAL_ONE) + RND * 8, (RAL_ONE) + RND * 2);
                RAL_TRANSLATE(&context, x, (RAL_ONE / 2), z);
                RAL_TRIANGLE(&context, -(RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2), -(RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2), 0xfcd0a1);
                RAL_TRIANGLE(&context, -(RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2), 0xb1b695);
                RAL_TRIANGLE(&context,  (RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2), 0x53917e);
                RAL_TRIANGLE(&context,  (RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2),  (RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), 0x63535b);
                RAL_TRIANGLE(&context,  (RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2), 0x6d1a36);
                RAL_TRIANGLE(&context,  (RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), 0xd4e09b);
                RAL_TRIANGLE(&context, -(RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2), 0xf6f4d2);
                RAL_TRIANGLE(&context, -(RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2), -(RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2), 0xcbdfbd);
                RAL_TRIANGLE(&context, -(RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2), -(RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2), 0xf19c79);
                RAL_TRIANGLE(&context, -(RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2), (RAL_ONE / 2),  (RAL_ONE / 2), (RAL_ONE / 2),-(RAL_ONE / 2), 0xa44a3f);
                RAL_TRIANGLE(&context,  (RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2), 0x5465ff);
                RAL_TRIANGLE(&context,  (RAL_ONE / 2),-(RAL_ONE / 2), (RAL_ONE / 2), -(RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2),  (RAL_ONE / 2),-(RAL_ONE / 2),-(RAL_ONE / 2), 0x788bff);
            }
        }
		
        // Scatter some pyramids around in the world.
		
        srand(seed);
        for (int i = 0; i < 20; ++i) {
            RAL_RESET(&context);
            RAL_SCALE(&context, RAL_ONE, RAL_ONE + RND * 3, RAL_ONE);
            RAL_ROTY(&context, RAL_FMUL(RND, RAL_PI));
            RAL_TRANSLATE(&context, (RAL_FMUL(RND, world_size) * 2) - world_size, 0, (RAL_FMUL(RND, world_size) * 2) - world_size);
            RAL_TRIANGLE(&context, 10, (RAL_ONE * 2), 10,-(RAL_ONE), 10, (RAL_ONE), (RAL_ONE), 10, (RAL_ONE), 0x004749);
            RAL_TRIANGLE(&context, 10, (RAL_ONE * 2), 10, (RAL_ONE), 10, (RAL_ONE), (RAL_ONE), 10,-(RAL_ONE), 0x00535a);
            RAL_TRIANGLE(&context, 10, (RAL_ONE * 2), 10, (RAL_ONE), 10,-(RAL_ONE),-(RAL_ONE), 10,-(RAL_ONE), 0x00746b);
            RAL_TRIANGLE(&context, 10, (RAL_ONE * 2), 10,-(RAL_ONE), 10,-(RAL_ONE),-(RAL_ONE), 10, (RAL_ONE), 0x00945c);
        }
		
        
		
        
        // Display the pixel buffer on the screen.
        SDL_Delay(1);
        SDL_RenderClear(renderer);
        SDL_UpdateTexture(texture, NULL, pixels, width * sizeof(uint32_t));
        SDL_RenderCopy(renderer, texture, NULL, NULL);
        SDL_RenderPresent(renderer);
		//printf("frame\n");
    }
}
