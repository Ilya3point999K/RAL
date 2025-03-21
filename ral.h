/*
        RAL - Render ALready, a fixed point software renderer.
        Basically a port of BOOTLEG3D by Benedict Henshaw to a fixed point
        rendering.
        by Ilya Efimov, 2025.
        CC0 1.0 Universal, Public Domain.
*/

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

//High precision trigonometry is quite a mess, if in your use-case there are
//glitches like 1-2 pixels on the right edge of the screen are not being
//rendered - define this, RAL_SET_FOV is the only place doubles are used.
#ifdef RAL_SET_FOV_WITH_DOUBLES  
#include <math.h>
#endif

#define RAL_CLAMP(X, lo, hi) (X < lo ? lo : X > hi ? hi : X)
#define RAL_MIN(a, b) ((a) > (b) ? (b) : (a))
#define RAL_MAX(a, b) ((a) < (b) ? (b) : (a))
#define RAL_ABS(a) ((a) > 0 ? (a) : -(a))
#define RAL_SIGN(a) (((a) > 0) - ((a) < 0))
typedef int64_t RAL_F;

typedef struct {
  RAL_F x, y;
} RAL_V2;
typedef struct {
  RAL_F x, y, z;
} RAL_V3;
typedef struct {
  RAL_F x, y, z, w;
} RAL_V4;
typedef struct {
  RAL_V4 p, d;
} RAL_R;
typedef struct {
  RAL_F m[4][4];
} RAL_M4;
typedef struct {
  RAL_V4 p[3];
} RAL_TRI;
typedef struct {
  RAL_V4 p[3];
  RAL_V2 uv[3];
} RAL_TRI_TEX;
typedef struct {
  RAL_V4 p;
  RAL_V4 n;
} RAL_PLANE;

typedef struct {
  uint32_t* pixels;
  int width;
  int height;
} RAL_TEXTURE;

#define RAL_MAX_NUM_POLYGON_VERTICES 10
#define RAL_MAX_NUM_POLYGON_TRIANGLES 8
typedef struct {
  RAL_V4 p[RAL_MAX_NUM_POLYGON_VERTICES];
  RAL_V2 uv[RAL_MAX_NUM_POLYGON_VERTICES];
  int n;
} RAL_POLYGON;

#define RAL_NUM_CLIP_PLANES 6
enum {
  RAL_LEFT_FRUSTUM_PLANE,
  RAL_RIGHT_FRUSTUM_PLANE,
  RAL_TOP_FRUSTUM_PLANE,
  RAL_BOTTOM_FRUSTUM_PLANE,
  RAL_NEAR_FRUSTUM_PLANE,
  RAL_FAR_FRUSTUM_PLANE
};

#define RAL_FP_SCALE 16
#define RAL_FP_UNIT (1 << RAL_FP_SCALE)
#define RAL_SHIFT_MASK ((1 << RAL_FP_SCALE) - 1)
#define RAL_ONE (1 << RAL_FP_SCALE)
#define RAL_HALF (RAL_ONE / 2)
#define RAL_QUARTER (RAL_ONE / 4)
#define RAL_EIGHTH (RAL_ONE / 8)

#define RAL_I2F(a) ((a) << RAL_FP_SCALE)
#define RAL_F2I(a) ((a) >> RAL_FP_SCALE)

#define RAL_D2F(a) ((RAL_F)((a) * (double)RAL_FP_UNIT))
#define RAL_F2D(a) ((double)(a) / (double)RAL_FP_UNIT)

#define RAL_FMUL(a, b) (RAL_F)(((int64_t)(a) * (int64_t)(b)) >> RAL_FP_SCALE)
#define RAL_FDIV(a, b) \
  ((b) == 0 ? 0 : (RAL_F)(((int64_t)(a) << RAL_FP_SCALE) / (int64_t)(b)))
// #define RAL_FMUL(a, b) (RAL_F)(((int64_t)(a) * (int64_t)(b)) / (1 <<
// RAL_FP_SCALE)) #define RAL_FDIV(a, b) ((b) == 0 ? 0 : (RAL_F)(((int64_t)(a) *
// (1 << RAL_FP_SCALE)) / (int64_t)(b)))

#define RAL_FLOOR(a) RAL_I2F(RAL_F2I((a)))
#define RAL_CEIL(a) ((a & 0xFFFF0000UL) + (a & 0x0000FFFFUL ? RAL_ONE : 0))
#define RAL_PI RAL_FDIV(355 * RAL_ONE, 112 * RAL_ONE)


typedef struct {
  RAL_PLANE clip_planes[RAL_NUM_CLIP_PLANES];
  RAL_M4 model, view, proj;
  RAL_V4 camera;
  RAL_F* depth;
  RAL_F near, far, fov;
  int width, height;
  uint32_t* pixels;
  uint8_t* object_buf;
} RAL_CONTEXT;

RAL_F RAL_FLERP(RAL_F a, RAL_F b, RAL_F t) { return a + RAL_FMUL((b - a), t); }

RAL_F RAL_FSIN(RAL_F x) {
  int sign = 1;

  if (x < 0)  // odd function
  {
    x *= -1;
    sign = -1;
  }

  x %= 2 * RAL_PI;

  if (x > RAL_PI) {
    x -= RAL_PI;
    sign *= -1;
  }

  x *= RAL_PI - x;

  return sign * (16 * x) / ((5 * RAL_PI * RAL_PI - 4 * x) / RAL_ONE);
}

RAL_F RAL_FCOS(RAL_F x) { return RAL_FSIN(x + (RAL_PI >> 1)); }

RAL_F RAL_FTAN(RAL_F x) { return RAL_FDIV(RAL_FSIN(x), RAL_FCOS(x)); }

RAL_F RAL_ATAN2(const RAL_F y, const RAL_F x) {
  RAL_F coeff_1 = RAL_PI / 4;
  RAL_F coeff_2 = 3 * coeff_1;
  RAL_F abs_y = RAL_ABS(y) + 1e-10;  // kludge to prevent 0/0 condition
  RAL_F angle;
  if (x >= 0) {
    RAL_F r = RAL_FDIV((x - abs_y), (x + abs_y));
    angle = coeff_1 - RAL_FMUL(coeff_1, r);
  } else {
    RAL_F r = RAL_FDIV((x + abs_y), (abs_y - x));
    angle = coeff_2 - RAL_FMUL(coeff_1, r);
  }
  if (y < 0)
    return (-angle);  // negate if in quad III or IV
  else
    return (angle);
}

RAL_F RAL_FATAN(RAL_F x) { return RAL_ATAN2(x, RAL_ONE); }

RAL_F RAL_ATAN2_HP(const RAL_F y, const RAL_F x) {
  RAL_F coeff_1 = RAL_PI / 4;
  RAL_F coeff_2 = 3 * coeff_1;
  RAL_F abs_y = RAL_ABS(y) + 1e-10;  // kludge to prevent 0/0 condition
  RAL_F angle;
  if (x >= 0) {
    RAL_F r = RAL_FDIV((x - abs_y), (x + abs_y));
    RAL_F rq = RAL_FMUL(RAL_FMUL(r, r), r);
    angle = RAL_FMUL(rq, 12865) - RAL_FMUL(64337, r) + coeff_1;
  } else {
    RAL_F r = RAL_FDIV((x + abs_y), (abs_y - x));
    RAL_F rq =
        RAL_FMUL(RAL_FMUL(r, r), r);
    angle = RAL_FMUL(rq, 12865) - RAL_FMUL(64337, r) + coeff_2;
  }
  if (y < 0)
    return (-angle);  // negate if in quad III or IV
  else
    return (angle);
}

RAL_F RAL_FATAN_HP(RAL_F x) { return RAL_ATAN2_HP(x, RAL_ONE); }

RAL_F RAL_SQRT(RAL_F A) {
  int invert = 0;
  int iter = RAL_FP_SCALE;
  int l, i;

  if (A < 0) return (-1);
  if (A == 0 || A == RAL_ONE) return (A);
  if (A < RAL_ONE && A > 6) {
    invert = 1;
    A = RAL_FDIV(RAL_ONE, A);
  }
  if (A > RAL_ONE) {
    int s = A;

    iter = 0;
    while (s > 0) {
      s >>= 2;
      iter++;
    }
  }

  l = (A >> 1) + 1;
  for (i = 0; i < iter; i++) l = (l + RAL_FDIV(A, l)) >> 1;
  if (invert) return (RAL_FDIV(RAL_ONE, l));
  return (l);
}

RAL_F RAL_V2_DISTANCE(RAL_V2 a, RAL_V2 b) {
  RAL_F x = b.x - a.x;
  RAL_F y = b.y - a.y;
  RAL_F r = RAL_SQRT(RAL_FMUL(x, x) + RAL_FMUL(y, y));
  return r;
}

RAL_V4 RAL_V4_ADD(RAL_V4 a, RAL_V4 b) {
  RAL_V4 r;
  r.x = a.x + b.x;
  r.y = a.y + b.y;
  r.z = a.z + b.z;
  return r;
}

RAL_V4 RAL_V4_ADDF(RAL_V4 a, RAL_F b) {
  RAL_V4 r;
  r.x = a.x + b;
  r.y = a.y + b;
  r.z = a.z + b;
  return r;
}

RAL_V4 RAL_V4_SUB(RAL_V4 a, RAL_V4 b) {
  RAL_V4 r;
  r.x = a.x - b.x;
  r.y = a.y - b.y;
  r.z = a.z - b.z;
  return r;
}

RAL_V4 RAL_V4_MUL(RAL_V4 a, RAL_V4 b) {
  RAL_V4 r;
  r.x = RAL_FMUL(a.x, b.x);
  r.y = RAL_FMUL(a.y, b.y);
  r.z = RAL_FMUL(a.z, b.z);
  return r;
}

RAL_V4 RAL_V4_MULF(RAL_V4 a, RAL_F b) {
  RAL_V4 r;
  r.x = RAL_FMUL(a.x, b);
  r.y = RAL_FMUL(a.y, b);
  r.z = RAL_FMUL(a.z, b);
  return r;
}

RAL_V4 RAL_V4_DIVF(RAL_V4 a, RAL_F b) {
  RAL_V4 r;
  r.x = RAL_FDIV(a.x, b);
  r.y = RAL_FDIV(a.y, b);
  r.z = RAL_FDIV(a.z, b);
  return r;
}

RAL_V4 RAL_V4_NORMALIZE(RAL_V4 a) {
  RAL_F norm =
      RAL_SQRT(RAL_FMUL(a.x, a.x) + RAL_FMUL(a.y, a.y) + RAL_FMUL(a.z, a.z));
  if (norm == 0) return a;
  return RAL_V4_DIVF(a, norm);
}
RAL_F RAL_V4_DOT(RAL_V4 a, RAL_V4 b) {
  RAL_F r = RAL_FMUL(a.x, b.x) + RAL_FMUL(a.y, b.y) + RAL_FMUL(a.z, b.z);
  return r;
}

RAL_F RAL_V4_DISTANCE(RAL_V4 a, RAL_V4 b) {
  RAL_F x = b.x - a.x;
  RAL_F y = b.y - a.y;
  RAL_F z = b.z - a.z;
  RAL_F r = RAL_SQRT(RAL_FMUL(x, x) + RAL_FMUL(y, y) + RAL_FMUL(z, z));
  return r;
}

RAL_F RAL_V4_SQDISTANCE(RAL_V4 a, RAL_V4 b) {
  RAL_F x = b.x - a.x;
  RAL_F y = b.y - a.y;
  RAL_F z = b.z - a.z;
  RAL_F r = RAL_FMUL(x, x) + RAL_FMUL(y, y) + RAL_FMUL(z, z);
  return r;
}

RAL_V4 RAL_V4_CROSS(RAL_V4 a, RAL_V4 b) {
  RAL_V4 r;
  r.x = RAL_FMUL(a.y, b.z) - RAL_FMUL(a.z, b.y);
  r.y = RAL_FMUL(a.z, b.x) - RAL_FMUL(a.x, b.z);
  r.z = RAL_FMUL(a.x, b.y) - RAL_FMUL(a.y, b.x);
  r.w = RAL_ONE;
  return r;
}

RAL_V4 RAL_V4_ROT(RAL_V4 a, RAL_V4 b, RAL_F angle) {
  // b = RAL_V4_NORMALIZE(b);
  RAL_V4 proj = RAL_V4_MULF(b, RAL_V4_DOT(a, b));
  return RAL_V4_ADD(
      RAL_V4_ADD(proj, RAL_V4_MULF(RAL_V4_SUB(a, proj), RAL_FCOS(angle))),
      RAL_V4_MULF(RAL_V4_CROSS(b, a), RAL_FSIN(angle)));
}

RAL_M4 RAL_M4_IDENT() {
  return (RAL_M4){{
      [0][0] = RAL_ONE,
      [1][1] = RAL_ONE,
      [2][2] = RAL_ONE,
      [3][3] = RAL_ONE,
  }};
}

RAL_M4 RAL_M4_ROTX(RAL_F a) {
  return (RAL_M4){{
      [0][0] = RAL_ONE,
      [1][1] = RAL_FCOS(a),
      [1][2] = RAL_FSIN(a),
      [2][1] = -RAL_FSIN(a),
      [2][2] = RAL_FCOS(a),
      [3][3] = RAL_ONE,
  }};
}

RAL_M4 RAL_M4_ROTY(RAL_F a) {
  return (RAL_M4){{
      [0][0] = RAL_FCOS(a),
      [0][2] = RAL_FSIN(a),
      [2][0] = -RAL_FSIN(a),
      [1][1] = RAL_ONE,
      [2][2] = RAL_FCOS(a),
      [3][3] = RAL_ONE,
  }};
}

RAL_M4 RAL_M4_ROTZ(RAL_F a) {
  return (RAL_M4){{
      [0][0] = RAL_FCOS(a),
      [0][1] = RAL_FSIN(a),
      [1][0] = -RAL_FSIN(a),
      [1][1] = RAL_FCOS(a),
      [2][2] = RAL_ONE,
      [3][3] = RAL_ONE,
  }};
}

RAL_M4 RAL_M4_TRANS(RAL_F x, RAL_F y, RAL_F z) {
  return (RAL_M4){{
      [0][0] = RAL_ONE,
      [1][1] = RAL_ONE,
      [2][2] = RAL_ONE,
      [3][3] = RAL_ONE,
      [3][0] = x,
      [3][1] = y,
      [3][2] = z,
  }};
}

RAL_M4 RAL_M4_SCALE(RAL_F x, RAL_F y, RAL_F z) {
  return (RAL_M4){{
      [0][0] = x,
      [1][1] = y,
      [2][2] = z,
      [3][3] = RAL_ONE,
  }};
}

RAL_M4 RAL_M4_PROJ(RAL_F fov, RAL_F aspect, RAL_F near, RAL_F far) {
  fov = RAL_FDIV(
      RAL_ONE,
      RAL_FTAN(RAL_FMUL(
          fov, RAL_FMUL(RAL_FDIV((RAL_ONE / 2), 180 * RAL_ONE), RAL_PI))));
  return (RAL_M4){{
      [0][0] = RAL_FDIV(fov, aspect),
      [1][1] = fov,
      [2][2] = RAL_FDIV(far, (far - near)),
      [3][2] = RAL_FDIV(RAL_FMUL(-far, near), (far - near)),
      [2][3] = RAL_ONE,
      [3][3] = 0,
  }};
}

RAL_M4 RAL_M4_MXM(RAL_M4 a, RAL_M4 b) {
  RAL_M4 matrix;
  for (int c = 0; c < 4; c++) {
    for (int r = 0; r < 4; r++) {
      matrix.m[r][c] =
          RAL_FMUL(a.m[r][0], b.m[0][c]) + RAL_FMUL(a.m[r][1], b.m[1][c]) +
          RAL_FMUL(a.m[r][2], b.m[2][c]) + RAL_FMUL(a.m[r][3], b.m[3][c]);
    }
  }
  return matrix;
}

RAL_V4 RAL_M4_MXV(RAL_M4 m, RAL_V4 v) {
  return (RAL_V4){
      RAL_FMUL(v.x, m.m[0][0]) + RAL_FMUL(v.y, m.m[1][0]) +
          RAL_FMUL(v.z, m.m[2][0]) + RAL_FMUL(v.w, m.m[3][0]),
      RAL_FMUL(v.x, m.m[0][1]) + RAL_FMUL(v.y, m.m[1][1]) +
          RAL_FMUL(v.z, m.m[2][1]) + RAL_FMUL(v.w, m.m[3][1]),
      RAL_FMUL(v.x, m.m[0][2]) + RAL_FMUL(v.y, m.m[1][2]) +
          RAL_FMUL(v.z, m.m[2][2]) + RAL_FMUL(v.w, m.m[3][2]),
      RAL_FMUL(v.x, m.m[0][3]) + RAL_FMUL(v.y, m.m[1][3]) +
          RAL_FMUL(v.z, m.m[2][3]) + RAL_FMUL(v.w, m.m[3][3]),
  };
}

RAL_M4 RAL_M4_QINV(RAL_M4 m) {
  RAL_M4 o = (RAL_M4){{
      [0][0] = m.m[0][0],
      [0][1] = m.m[1][0],
      [0][2] = m.m[2][0],
      [0][3] = 0,
      [1][0] = m.m[0][1],
      [1][1] = m.m[1][1],
      [1][2] = m.m[2][1],
      [1][3] = 0,
      [2][0] = m.m[0][2],
      [2][1] = m.m[1][2],
      [2][2] = m.m[2][2],
      [2][3] = 0,
  }};
  o.m[3][0] =
      -(RAL_FMUL(m.m[3][0], o.m[0][0]) + RAL_FMUL(m.m[3][1], o.m[1][0]) +
        RAL_FMUL(m.m[3][2], o.m[2][0]));
  o.m[3][1] =
      -(RAL_FMUL(m.m[3][0], o.m[0][1]) + RAL_FMUL(m.m[3][1], o.m[1][1]) +
        RAL_FMUL(m.m[3][2], o.m[2][1]));
  o.m[3][2] =
      -(RAL_FMUL(m.m[3][0], o.m[0][2]) + RAL_FMUL(m.m[3][1], o.m[1][2]) +
        RAL_FMUL(m.m[3][2], o.m[2][2]));
  o.m[3][3] = RAL_ONE;
  return o;
}

uint32_t RAL_PACK_RGB(uint8_t r, uint8_t g, uint8_t b) {
  uint8_t alpha = 255;
  return (alpha << 24) + (b << 16) + (g << 8) + r;
}

uint32_t RAL_SAMPLE_TEX(RAL_TEXTURE* texture, RAL_F u, RAL_F v) {
  int u_texture =
      RAL_F2I(RAL_FMUL(u, RAL_I2F(texture->width - 1))) % texture->width;
  int v_texture =
      RAL_F2I(RAL_FMUL(v, RAL_I2F(texture->width - 1))) % texture->height;
  int index = v_texture * texture->width + u_texture;

  return texture->pixels[index];
}

RAL_M4 RAL_POINTAT(RAL_V4 pos, RAL_V4 target, RAL_V4 up) {
  RAL_V4 forward = RAL_V4_SUB(target, pos);
  forward = RAL_V4_NORMALIZE(forward);
  RAL_V4 a = RAL_V4_MULF(forward, RAL_V4_DOT(up, forward));
  up = RAL_V4_NORMALIZE(RAL_V4_SUB(up, a));
  RAL_V4 right = RAL_V4_CROSS(up, forward);
  return (RAL_M4){{
      [0][0] = right.x,
      [0][1] = right.y,
      [0][2] = right.z,
      [0][3] = 0,
      [1][0] = up.x,
      [1][1] = up.y,
      [1][2] = up.z,
      [1][3] = 0,
      [2][0] = forward.x,
      [2][1] = forward.y,
      [2][2] = forward.z,
      [2][3] = 0,
      [3][0] = pos.x,
      [3][1] = pos.y,
      [3][2] = pos.z,
      [3][3] = RAL_ONE,
  }};
}

RAL_POLYGON RAL_TRI2POLY(RAL_V4 v0, RAL_V4 v1, RAL_V4 v2, RAL_V2 t0, RAL_V2 t1,
                         RAL_V2 t2) {
  RAL_POLYGON polygon = {.p =
                             {
                                 (RAL_V4){v0.x, v0.y, v0.z},
                                 (RAL_V4){v1.x, v1.y, v1.z},
                                 (RAL_V4){v2.x, v2.y, v2.z},
                             },
                         .uv = {t0, t1, t2},
                         .n = 3};

  return polygon;
}

void RAL_CLIPPING_WORK(RAL_CONTEXT* context, RAL_POLYGON* polygon, int plane) {
  RAL_V4 plane_p = context->clip_planes[plane].p;
  RAL_V4 plane_n = context->clip_planes[plane].n;

  RAL_V4 inside_p[RAL_MAX_NUM_POLYGON_VERTICES];
  RAL_V2 inside_uv[RAL_MAX_NUM_POLYGON_VERTICES];
  int n_inside = 0;

  RAL_V4* p_curr = &polygon->p[0];
  RAL_V4* p_prev = &polygon->p[polygon->n - 1];
  RAL_V2* uv_curr = &polygon->uv[0];
  RAL_V2* uv_prev = &polygon->uv[polygon->n - 1];

  RAL_F dot_prev = RAL_V4_DOT(RAL_V4_SUB(*p_prev, plane_p), plane_n);

  while (p_curr != &polygon->p[polygon->n]) {
    RAL_F dot_curr = RAL_V4_DOT(RAL_V4_SUB(*p_curr, plane_p), plane_n);

    if (RAL_FMUL(dot_curr, dot_prev) < 0) {
      RAL_F t = RAL_FDIV(dot_prev, (dot_prev - dot_curr));

      RAL_V4 intersection = {
          RAL_FLERP(p_prev->x, p_curr->x, t),
          RAL_FLERP(p_prev->y, p_curr->y, t),
          RAL_FLERP(p_prev->z, p_curr->z, t),
      };

      RAL_V2 interpolated_uv = {.x = RAL_FLERP(uv_prev->x, uv_curr->x, t),
                                .y = RAL_FLERP(uv_prev->y, uv_curr->y, t)};

      inside_p[n_inside] = intersection;
      inside_uv[n_inside] = interpolated_uv;
      n_inside++;
    }

    if (dot_curr > 0) {
      inside_p[n_inside] = *p_curr;
      inside_uv[n_inside] = *uv_curr;
      n_inside++;
    }

    dot_prev = dot_curr;
    p_prev = p_curr;
    uv_prev = uv_curr;
    p_curr++;
    uv_curr++;
  }

  for (int i = 0; i < n_inside; ++i) polygon->p[i] = inside_p[i];

  for (int i = 0; i < n_inside; ++i) polygon->uv[i] = inside_uv[i];

  polygon->n = n_inside;
}

void RAL_DO_CLIP(RAL_CONTEXT* context, RAL_POLYGON* polygon) {
  RAL_CLIPPING_WORK(context, polygon, RAL_LEFT_FRUSTUM_PLANE);
  RAL_CLIPPING_WORK(context, polygon, RAL_RIGHT_FRUSTUM_PLANE);

  RAL_CLIPPING_WORK(context, polygon, RAL_TOP_FRUSTUM_PLANE);
  RAL_CLIPPING_WORK(context, polygon, RAL_BOTTOM_FRUSTUM_PLANE);

  RAL_CLIPPING_WORK(context, polygon, RAL_NEAR_FRUSTUM_PLANE);
  RAL_CLIPPING_WORK(context, polygon, RAL_FAR_FRUSTUM_PLANE);
}

void RAL_POLY2TRI(RAL_POLYGON* polygon, RAL_TRI_TEX* triangles, int* n) {
  *n = polygon->n - 2;

  for (int i = 0; i < *n; ++i) {
    RAL_V4 v0 = polygon->p[0];
    RAL_V4 v1 = polygon->p[i + 1];
    RAL_V4 v2 = polygon->p[i + 2];

    triangles[i].p[0] = (RAL_V4){v0.x, v0.y, v0.z, RAL_ONE};
    triangles[i].p[1] = (RAL_V4){v1.x, v1.y, v1.z, RAL_ONE};
    triangles[i].p[2] = (RAL_V4){v2.x, v2.y, v2.z, RAL_ONE};

    triangles[i].uv[0] = polygon->uv[0];
    triangles[i].uv[1] = polygon->uv[i + 1];
    triangles[i].uv[2] = polygon->uv[i + 2];
  }
}

void RAL_RASTERIZE(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az, RAL_F bx,
                   RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy, RAL_F cz,
                   uint32_t c) {
  RAL_F t = 0;
  if (ay > by) {
    t = ax;
    ax = bx;
    bx = t;
    t = ay;
    ay = by;
    by = t;
    t = az;
    az = bz;
    bz = t;
  }
  if (ay > cy) {
    t = ax;
    ax = cx;
    cx = t;
    t = ay;
    ay = cy;
    cy = t;
    t = az;
    az = cz;
    cz = t;
  }
  if (by > cy) {
    t = bx;
    bx = cx;
    cx = t;
    t = by;
    by = cy;
    cy = t;
    t = bz;
    bz = cz;
    cz = t;
  }
  RAL_F long_x = ax, long_step_x = RAL_FDIV((cx - ax), (cy - ay));
  RAL_F long_z = az, long_step_z = RAL_FDIV((cz - az), (cy - ay));
  RAL_F short_x, short_step_x, short_z, short_step_z;
#define INNER                                                      \
  {                                                                \
    RAL_F left_x = long_x, right_x = short_x;                      \
    RAL_F left_z = long_z, right_z = short_z;                      \
    if (left_x > right_x) {                                        \
      left_x = short_x;                                            \
      right_x = long_x;                                            \
      left_z = short_z;                                            \
      right_z = long_z;                                            \
    }                                                              \
    RAL_F step = RAL_FDIV((right_z - left_z), (right_x - left_x)); \
    int stride_offset = y * context->width;                        \
    int lx = RAL_F2I(left_x);                                      \
    int rx = RAL_F2I(right_x);                                     \
    uint32_t* pixel = context->pixels + (lx + stride_offset);      \
    uint32_t* end = context->pixels + (rx + stride_offset);        \
    RAL_F* depth = context->depth + (lx + stride_offset);          \
    RAL_F d = left_z;                                              \
    while (pixel < end) {                                          \
      if (d < *depth) {                                            \
        *depth = d;                                                \
        *pixel = c;                                                \
      }                                                            \
      d += step;                                                   \
      ++pixel;                                                     \
      ++depth;                                                     \
    }                                                              \
    long_x += long_step_x;                                         \
    long_z += long_step_z;                                         \
    short_x += short_step_x;                                       \
    short_z += short_step_z;                                       \
  }
  short_x = ax, short_step_x = RAL_FDIV((bx - ax), (by - ay));
  short_z = az, short_step_z = RAL_FDIV((bz - az), (by - ay));
  int sy = RAL_F2I(ay);
  int ey = RAL_F2I(by);
  for (int y = sy; y < ey; ++y) INNER
  short_x = bx, short_step_x = RAL_FDIV((cx - bx), (cy - by));
  short_z = bz, short_step_z = RAL_FDIV((cz - bz), (cy - by));
  sy = RAL_F2I(by);
  ey = RAL_F2I(cy);
  for (int y = sy; y < ey; ++y) INNER
#undef INNER
}

void RAL_RASTERIZE_OBJ(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az,
                       RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy,
                       RAL_F cz, uint32_t c, uint8_t obj) {
  RAL_F t = 0;
  if (ay > by) {
    t = ax;
    ax = bx;
    bx = t;
    t = ay;
    ay = by;
    by = t;
    t = az;
    az = bz;
    bz = t;
  }
  if (ay > cy) {
    t = ax;
    ax = cx;
    cx = t;
    t = ay;
    ay = cy;
    cy = t;
    t = az;
    az = cz;
    cz = t;
  }
  if (by > cy) {
    t = bx;
    bx = cx;
    cx = t;
    t = by;
    by = cy;
    cy = t;
    t = bz;
    bz = cz;
    cz = t;
  }

  RAL_F long_x = ax, long_step_x = RAL_FDIV((cx - ax), (cy - ay));
  RAL_F long_z = az, long_step_z = RAL_FDIV((cz - az), (cy - ay));
  RAL_F short_x, short_step_x, short_z, short_step_z;
#define INNER                                                      \
  {                                                                \
    RAL_F left_x = long_x, right_x = short_x;                      \
    RAL_F left_z = long_z, right_z = short_z;                      \
    if (left_x > right_x) {                                        \
      left_x = short_x;                                            \
      right_x = long_x;                                            \
      left_z = short_z;                                            \
      right_z = long_z;                                            \
    }                                                              \
    RAL_F step = RAL_FDIV((right_z - left_z), (right_x - left_x)); \
    int stride_offset = y * context->width;                        \
    int lx = RAL_F2I(left_x);                                      \
    int rx = RAL_F2I(right_x);                                     \
    uint32_t* pixel = context->pixels + (lx + stride_offset);      \
    uint32_t* end = context->pixels + (rx + stride_offset);        \
    uint8_t* object = context->object_buf + (lx + stride_offset);  \
    RAL_F* depth = context->depth + (lx + stride_offset);          \
    RAL_F d = left_z;                                              \
    while (pixel < end) {                                          \
      if (d < *depth) {                                            \
        *depth = d;                                                \
        *pixel = c;                                                \
        *object = obj;                                             \
      }                                                            \
      d += step;                                                   \
      ++pixel;                                                     \
      ++depth;                                                     \
      ++object;                                                    \
    }                                                              \
    long_x += long_step_x;                                         \
    long_z += long_step_z;                                         \
    short_x += short_step_x;                                       \
    short_z += short_step_z;                                       \
  }
  short_x = ax, short_step_x = RAL_FDIV((bx - ax), (by - ay));
  short_z = az, short_step_z = RAL_FDIV((bz - az), (by - ay));
  int sy = RAL_F2I(ay);
  int ey = RAL_F2I(by);
  for (int y = sy; y < ey; ++y) INNER
  short_x = bx, short_step_x = RAL_FDIV((cx - bx), (cy - by));
  short_z = bz, short_step_z = RAL_FDIV((cz - bz), (cy - by));
  sy = RAL_F2I(by);
  ey = RAL_F2I(cy);
  for (int y = sy; y < ey; ++y) INNER
#undef INNER
}

void RAL_RASTERIZE_TEX(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az,
                       RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy,
                       RAL_F cz, RAL_F au, RAL_F av, RAL_F bu, RAL_F bv,
                       RAL_F cu, RAL_F cv, RAL_TEXTURE* texture) {
  RAL_F t = 0;
  if (ay > by) {
    t = ax;
    ax = bx;
    bx = t;
    t = ay;
    ay = by;
    by = t;
    t = az;
    az = bz;
    bz = t;
    t = au;
    au = bu;
    bu = t;
    t = av;
    av = bv;
    bv = t;
  }
  if (ay > cy) {
    t = ax;
    ax = cx;
    cx = t;
    t = ay;
    ay = cy;
    cy = t;
    t = az;
    az = cz;
    cz = t;
    t = au;
    au = cu;
    cu = t;
    t = av;
    av = cv;
    cv = t;
  }
  if (by > cy) {
    t = bx;
    bx = cx;
    cx = t;
    t = by;
    by = cy;
    cy = t;
    t = bz;
    bz = cz;
    cz = t;
    t = bu;
    bu = cu;
    cu = t;
    t = bv;
    bv = cv;
    cv = t;
  }
  RAL_F a_oow = RAL_FDIV(RAL_ONE, az);
  RAL_F b_oow = RAL_FDIV(RAL_ONE, bz);
  RAL_F c_oow = RAL_FDIV(RAL_ONE, cz);

  RAL_F a_uow = RAL_FMUL(au, a_oow);
  RAL_F a_vow = RAL_FMUL(av, a_oow);
  RAL_F b_uow = RAL_FMUL(bu, b_oow);
  RAL_F b_vow = RAL_FMUL(bv, b_oow);
  RAL_F c_uow = RAL_FMUL(cu, c_oow);
  RAL_F c_vow = RAL_FMUL(cv, c_oow);

  RAL_F long_x = ax, long_step_x = RAL_FDIV((cx - ax), (cy - ay));
  RAL_F long_z = az, long_step_z = RAL_FDIV((cz - az), (cy - ay));
  RAL_F long_uow = a_uow, long_step_uow = RAL_FDIV((c_uow - a_uow), (cy - ay));
  RAL_F long_vow = a_vow, long_step_vow = RAL_FDIV((c_vow - a_vow), (cy - ay));
  RAL_F long_oow = a_oow, long_step_oow = RAL_FDIV((c_oow - a_oow), (cy - ay));

  RAL_F short_x, short_step_x, short_z, short_step_z;
  RAL_F short_uow, short_step_uow, short_vow, short_step_vow, short_oow,
      short_step_oow;
#define INNER                                                                \
  {                                                                          \
    RAL_F left_x = long_x, right_x = short_x;                                \
    RAL_F left_z = long_z, right_z = short_z;                                \
    RAL_F left_uow = long_uow, right_uow = short_uow;                        \
    RAL_F left_vow = long_vow, right_vow = short_vow;                        \
    RAL_F left_oow = long_oow, right_oow = short_oow;                        \
    if (left_x > right_x) {                                                  \
      RAL_F tmp = left_x;                                                    \
      left_x = right_x;                                                      \
      right_x = tmp;                                                         \
      tmp = left_z;                                                          \
      left_z = right_z;                                                      \
      right_z = tmp;                                                         \
      tmp = left_uow;                                                        \
      left_uow = right_uow;                                                  \
      right_uow = tmp;                                                       \
      tmp = left_vow;                                                        \
      left_vow = right_vow;                                                  \
      right_vow = tmp;                                                       \
      tmp = left_oow;                                                        \
      left_oow = right_oow;                                                  \
      right_oow = tmp;                                                       \
    }                                                                        \
    RAL_F delta_x = right_x - left_x;                                        \
    if (delta_x > 0) {                                                       \
      RAL_F step_z = RAL_FDIV((right_z - left_z), delta_x);                  \
      RAL_F step_uow = RAL_FDIV((right_uow - left_uow), delta_x);            \
      RAL_F step_vow = RAL_FDIV((right_vow - left_vow), delta_x);            \
      RAL_F step_oow = RAL_FDIV((right_oow - left_oow), delta_x);            \
      int y_int = y;                                                         \
      int stride_offset = y_int * context->width;                            \
      uint32_t* pixel = context->pixels + (RAL_F2I(left_x) + stride_offset); \
      uint32_t* end = context->pixels + (RAL_F2I(right_x) + stride_offset);  \
      RAL_F* depth = context->depth + (RAL_F2I(left_x) + stride_offset);     \
      RAL_F d = left_z;                                                      \
      RAL_F uow = left_uow;                                                  \
      RAL_F vow = left_vow;                                                  \
      RAL_F oow = left_oow;                                                  \
      while (pixel < end) {                                                  \
        if (d < *depth) {                                                    \
          RAL_F u = RAL_FDIV(uow, oow);                                      \
          RAL_F v = RAL_FDIV(vow, oow);                                      \
          *pixel = RAL_SAMPLE_TEX(texture, u, v);                            \
          *depth = d;                                                        \
        }                                                                    \
        d += step_z;                                                         \
        uow += step_uow;                                                     \
        vow += step_vow;                                                     \
        oow += step_oow;                                                     \
        pixel++;                                                             \
        depth++;                                                             \
      }                                                                      \
    }                                                                        \
    long_x += long_step_x;                                                   \
    long_z += long_step_z;                                                   \
    long_uow += long_step_uow;                                               \
    long_vow += long_step_vow;                                               \
    long_oow += long_step_oow;                                               \
    short_x += short_step_x;                                                 \
    short_z += short_step_z;                                                 \
    short_uow += short_step_uow;                                             \
    short_vow += short_step_vow;                                             \
    short_oow += short_step_oow;                                             \
  }
  short_x = ax, short_step_x = RAL_FDIV((bx - ax), (by - ay));
  short_z = az, short_step_z = RAL_FDIV((bz - az), (by - ay));
  short_uow = a_uow, short_step_uow = RAL_FDIV((b_uow - a_uow), (by - ay));
  short_vow = a_vow, short_step_vow = RAL_FDIV((b_vow - a_vow), (by - ay));
  short_oow = a_oow, short_step_oow = RAL_FDIV((b_oow - a_oow), (by - ay));
  int sy = RAL_F2I(ay);
  int ey = RAL_F2I(by);
  for (int y = sy; y < ey; ++y) INNER
  short_x = bx, short_step_x = RAL_FDIV((cx - bx), (cy - by));
  short_z = bz, short_step_z = RAL_FDIV((cz - bz), (cy - by));
  short_uow = b_uow, short_step_uow = RAL_FDIV((c_uow - b_uow), (cy - by));
  short_vow = b_vow, short_step_vow = RAL_FDIV((c_vow - b_vow), (cy - by));
  short_oow = b_oow, short_step_oow = RAL_FDIV((c_oow - b_oow), (cy - by));
  sy = RAL_F2I(by);
  ey = RAL_F2I(cy);
  for (int y = sy; y < ey; ++y) INNER
#undef INNER
}

void RAL_RASTERIZE_TEX_OBJ(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az,
                       RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy,
                       RAL_F cz, RAL_F au, RAL_F av, RAL_F bu, RAL_F bv,
                       RAL_F cu, RAL_F cv, RAL_TEXTURE* texture, uint8_t obj) {
  RAL_F t = 0;
  if (ay > by) {
    t = ax;
    ax = bx;
    bx = t;
    t = ay;
    ay = by;
    by = t;
    t = az;
    az = bz;
    bz = t;
    t = au;
    au = bu;
    bu = t;
    t = av;
    av = bv;
    bv = t;
  }
  if (ay > cy) {
    t = ax;
    ax = cx;
    cx = t;
    t = ay;
    ay = cy;
    cy = t;
    t = az;
    az = cz;
    cz = t;
    t = au;
    au = cu;
    cu = t;
    t = av;
    av = cv;
    cv = t;
  }
  if (by > cy) {
    t = bx;
    bx = cx;
    cx = t;
    t = by;
    by = cy;
    cy = t;
    t = bz;
    bz = cz;
    cz = t;
    t = bu;
    bu = cu;
    cu = t;
    t = bv;
    bv = cv;
    cv = t;
  }
  RAL_F a_oow = RAL_FDIV(RAL_ONE, az);
  RAL_F b_oow = RAL_FDIV(RAL_ONE, bz);
  RAL_F c_oow = RAL_FDIV(RAL_ONE, cz);

  RAL_F a_uow = RAL_FMUL(au, a_oow);
  RAL_F a_vow = RAL_FMUL(av, a_oow);
  RAL_F b_uow = RAL_FMUL(bu, b_oow);
  RAL_F b_vow = RAL_FMUL(bv, b_oow);
  RAL_F c_uow = RAL_FMUL(cu, c_oow);
  RAL_F c_vow = RAL_FMUL(cv, c_oow);

  RAL_F long_x = ax, long_step_x = RAL_FDIV((cx - ax), (cy - ay));
  RAL_F long_z = az, long_step_z = RAL_FDIV((cz - az), (cy - ay));
  RAL_F long_uow = a_uow, long_step_uow = RAL_FDIV((c_uow - a_uow), (cy - ay));
  RAL_F long_vow = a_vow, long_step_vow = RAL_FDIV((c_vow - a_vow), (cy - ay));
  RAL_F long_oow = a_oow, long_step_oow = RAL_FDIV((c_oow - a_oow), (cy - ay));

  RAL_F short_x, short_step_x, short_z, short_step_z;
  RAL_F short_uow, short_step_uow, short_vow, short_step_vow, short_oow,
      short_step_oow;
#define INNER                                                                \
  {                                                                          \
    RAL_F left_x = long_x, right_x = short_x;                                \
    RAL_F left_z = long_z, right_z = short_z;                                \
    RAL_F left_uow = long_uow, right_uow = short_uow;                        \
    RAL_F left_vow = long_vow, right_vow = short_vow;                        \
    RAL_F left_oow = long_oow, right_oow = short_oow;                        \
    if (left_x > right_x) {                                                  \
      RAL_F tmp = left_x;                                                    \
      left_x = right_x;                                                      \
      right_x = tmp;                                                         \
      tmp = left_z;                                                          \
      left_z = right_z;                                                      \
      right_z = tmp;                                                         \
      tmp = left_uow;                                                        \
      left_uow = right_uow;                                                  \
      right_uow = tmp;                                                       \
      tmp = left_vow;                                                        \
      left_vow = right_vow;                                                  \
      right_vow = tmp;                                                       \
      tmp = left_oow;                                                        \
      left_oow = right_oow;                                                  \
      right_oow = tmp;                                                       \
    }                                                                        \
    RAL_F delta_x = right_x - left_x;                                        \
    if (delta_x > 0) {                                                       \
      RAL_F step_z = RAL_FDIV((right_z - left_z), delta_x);                  \
      RAL_F step_uow = RAL_FDIV((right_uow - left_uow), delta_x);            \
      RAL_F step_vow = RAL_FDIV((right_vow - left_vow), delta_x);            \
      RAL_F step_oow = RAL_FDIV((right_oow - left_oow), delta_x);            \
      int y_int = y;                                                         \
      int stride_offset = y_int * context->width;                            \
      uint32_t* pixel = context->pixels + (RAL_F2I(left_x) + stride_offset); \
      uint32_t* end = context->pixels + (RAL_F2I(right_x) + stride_offset);  \
      RAL_F* depth = context->depth + (RAL_F2I(left_x) + stride_offset);     \
      uint8_t* op = context->object_buf + (RAL_F2I(left_x) + stride_offset); \
      RAL_F d = left_z;                                                      \
      RAL_F uow = left_uow;                                                  \
      RAL_F vow = left_vow;                                                  \
      RAL_F oow = left_oow;                                                  \
      while (pixel < end) {                                                  \
        if (d < *depth) {                                                    \
          RAL_F u = RAL_FDIV(uow, oow);                                      \
          RAL_F v = RAL_FDIV(vow, oow);                                      \
          *pixel = RAL_SAMPLE_TEX(texture, u, v);                            \
          *depth = d;                                                        \
          *op = obj;                                                         \
        }                                                                    \
        d += step_z;                                                         \
        uow += step_uow;                                                     \
        vow += step_vow;                                                     \
        oow += step_oow;                                                     \
        pixel++;                                                             \
        depth++;                                                             \
        op++;                                                                \
      }                                                                      \
    }                                                                        \
    long_x += long_step_x;                                                   \
    long_z += long_step_z;                                                   \
    long_uow += long_step_uow;                                               \
    long_vow += long_step_vow;                                               \
    long_oow += long_step_oow;                                               \
    short_x += short_step_x;                                                 \
    short_z += short_step_z;                                                 \
    short_uow += short_step_uow;                                             \
    short_vow += short_step_vow;                                             \
    short_oow += short_step_oow;                                             \
  }
  short_x = ax, short_step_x = RAL_FDIV((bx - ax), (by - ay));
  short_z = az, short_step_z = RAL_FDIV((bz - az), (by - ay));
  short_uow = a_uow, short_step_uow = RAL_FDIV((b_uow - a_uow), (by - ay));
  short_vow = a_vow, short_step_vow = RAL_FDIV((b_vow - a_vow), (by - ay));
  short_oow = a_oow, short_step_oow = RAL_FDIV((b_oow - a_oow), (by - ay));
  int sy = RAL_F2I(ay);
  int ey = RAL_F2I(by);
  for (int y = sy; y < ey; ++y) INNER
  short_x = bx, short_step_x = RAL_FDIV((cx - bx), (cy - by));
  short_z = bz, short_step_z = RAL_FDIV((cz - bz), (cy - by));
  short_uow = b_uow, short_step_uow = RAL_FDIV((c_uow - b_uow), (cy - by));
  short_vow = b_vow, short_step_vow = RAL_FDIV((c_vow - b_vow), (cy - by));
  short_oow = b_oow, short_step_oow = RAL_FDIV((c_oow - b_oow), (cy - by));
  sy = RAL_F2I(by);
  ey = RAL_F2I(cy);
  for (int y = sy; y < ey; ++y) INNER
#undef INNER
}

void RAL_TRIANGLE(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az, RAL_F bx,
                  RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy, RAL_F cz,
                  uint32_t c) {
  RAL_TRI_TEX t = (RAL_TRI_TEX){
      {{ax, ay, az, RAL_ONE}, {bx, by, bz, RAL_ONE}, {cx, cy, cz, RAL_ONE}},
      {{0, 0}, {0, 0}, {0, 0}}};

  t.p[0] = RAL_M4_MXV(context->model, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->model, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->model, t.p[2]);

  t.p[0] = RAL_M4_MXV(context->view, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->view, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->view, t.p[2]);
  if (t.p[0].z > context->far || t.p[1].z > context->far ||
      t.p[2].z > context->far)
    return;

  RAL_POLYGON polygon =
      RAL_TRI2POLY(t.p[0], t.p[1], t.p[2], t.uv[0], t.uv[1], t.uv[2]);

  RAL_DO_CLIP(context, &polygon);

  RAL_TRI_TEX clipped_tris[RAL_MAX_NUM_POLYGON_TRIANGLES];
  int n_clipped_tris = 0;

  RAL_POLY2TRI(&polygon, clipped_tris, &n_clipped_tris);
  RAL_F xs = context->width * RAL_ONE / 2;
  RAL_F ys = context->height * RAL_ONE / 2;

  for (int i = 0; i < n_clipped_tris; ++i) {
    RAL_TRI_TEX triangle = clipped_tris[i];
    for (int j = 0; j < 3; j++) {
      triangle.p[j] = RAL_M4_MXV(context->proj, triangle.p[j]);
      if (triangle.p[j].w != 0) {
        RAL_F invZ = RAL_FDIV(RAL_ONE, triangle.p[j].w);
        triangle.p[j].x = RAL_FMUL(triangle.p[j].x, invZ);
        triangle.p[j].y = RAL_FMUL(triangle.p[j].y, invZ);
        triangle.p[j].z = RAL_FMUL(triangle.p[j].z, invZ);
      }

      triangle.p[j].x = RAL_FMUL(triangle.p[j].x, xs);
      triangle.p[j].y = RAL_FMUL(-triangle.p[j].y, ys);

      triangle.p[j].x += xs;
      triangle.p[j].y += ys;
    }
    //Very ugly, sorry, the same reason RAL_SET_FOV_WITH_DOUBLES exists.
    triangle.p[0].x = RAL_CLAMP(triangle.p[0].x, 0, context->width * RAL_ONE);
    triangle.p[0].y = RAL_CLAMP(triangle.p[0].y, 0, context->height * RAL_ONE);
    triangle.p[1].x = RAL_CLAMP(triangle.p[1].x, 0, context->width * RAL_ONE);
    triangle.p[1].y = RAL_CLAMP(triangle.p[1].y, 0, context->height * RAL_ONE);
    triangle.p[2].x = RAL_CLAMP(triangle.p[2].x, 0, context->width * RAL_ONE);
    triangle.p[2].y = RAL_CLAMP(triangle.p[2].y, 0, context->height * RAL_ONE);

    RAL_RASTERIZE(context, triangle.p[0].x, triangle.p[0].y, triangle.p[0].w,
                  triangle.p[1].x, triangle.p[1].y, triangle.p[1].w,
                  triangle.p[2].x, triangle.p[2].y, triangle.p[2].w, c);
  }
}

void RAL_TRIANGLE_OBJ(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az,
                      RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy,
                      RAL_F cz, uint32_t c, uint8_t obj) {
  RAL_TRI_TEX t = (RAL_TRI_TEX){
      {{ax, ay, az, RAL_ONE}, {bx, by, bz, RAL_ONE}, {cx, cy, cz, RAL_ONE}},
      {{0, 0}, {0, 0}, {0, 0}}};

  t.p[0] = RAL_M4_MXV(context->model, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->model, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->model, t.p[2]);

  t.p[0] = RAL_M4_MXV(context->view, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->view, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->view, t.p[2]);
  if (t.p[0].z > context->far || t.p[1].z > context->far ||
      t.p[2].z > context->far)
    return;

  RAL_POLYGON polygon =
      RAL_TRI2POLY(t.p[0], t.p[1], t.p[2], t.uv[0], t.uv[1], t.uv[2]);

  RAL_DO_CLIP(context, &polygon);

  RAL_TRI_TEX clipped_tris[RAL_MAX_NUM_POLYGON_TRIANGLES];
  int n_clipped_tris = 0;

  RAL_POLY2TRI(&polygon, clipped_tris, &n_clipped_tris);
  RAL_F xs = context->width * RAL_ONE / 2;
  RAL_F ys = context->height * RAL_ONE / 2;

  for (int i = 0; i < n_clipped_tris; ++i) {
    RAL_TRI_TEX triangle = clipped_tris[i];
    for (int j = 0; j < 3; j++) {
      triangle.p[j] = RAL_M4_MXV(context->proj, triangle.p[j]);
      if (triangle.p[j].w != 0) {
        RAL_F invZ = RAL_FDIV(RAL_ONE, triangle.p[j].w);
        triangle.p[j].x = RAL_FMUL(triangle.p[j].x, invZ);
        triangle.p[j].y = RAL_FMUL(triangle.p[j].y, invZ);
        triangle.p[j].z = RAL_FMUL(triangle.p[j].z, invZ);
      }

      triangle.p[j].x = RAL_FMUL(triangle.p[j].x, xs);
      triangle.p[j].y = RAL_FMUL(-triangle.p[j].y, ys);

      triangle.p[j].x += xs;
      triangle.p[j].y += ys;
    }
    triangle.p[0].x = RAL_CLAMP(triangle.p[0].x, 0, context->width * RAL_ONE);
    triangle.p[0].y = RAL_CLAMP(triangle.p[0].y, 0, context->height * RAL_ONE);
    triangle.p[1].x = RAL_CLAMP(triangle.p[1].x, 0, context->width * RAL_ONE);
    triangle.p[1].y = RAL_CLAMP(triangle.p[1].y, 0, context->height * RAL_ONE);
    triangle.p[2].x = RAL_CLAMP(triangle.p[2].x, 0, context->width * RAL_ONE);
    triangle.p[2].y = RAL_CLAMP(triangle.p[2].y, 0, context->height * RAL_ONE);

    RAL_RASTERIZE_OBJ(context, triangle.p[0].x, triangle.p[0].y,
                      triangle.p[0].w, triangle.p[1].x, triangle.p[1].y,
                      triangle.p[1].w, triangle.p[2].x, triangle.p[2].y,
                      triangle.p[2].w, c, obj);
  }
}

void RAL_TRIANGLE_TEX(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az,
                      RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy,
                      RAL_F cz, RAL_F au, RAL_F av, RAL_F bu, RAL_F bv,
                      RAL_F cu, RAL_F cv, RAL_TEXTURE* texture) {
  RAL_TRI_TEX t = (RAL_TRI_TEX){
      {{ax, ay, az, RAL_ONE}, {bx, by, bz, RAL_ONE}, {cx, cy, cz, RAL_ONE}},
      {{au, av}, {bu, bv}, {cu, cv}}};

  t.p[0] = RAL_M4_MXV(context->model, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->model, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->model, t.p[2]);

  t.p[0] = RAL_M4_MXV(context->view, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->view, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->view, t.p[2]);
  if (t.p[0].z > context->far || t.p[1].z > context->far ||
      t.p[2].z > context->far)
    return;

  RAL_POLYGON polygon =
      RAL_TRI2POLY(t.p[0], t.p[1], t.p[2], t.uv[0], t.uv[1], t.uv[2]);

  RAL_DO_CLIP(context, &polygon);

  RAL_TRI_TEX clipped_tris[RAL_MAX_NUM_POLYGON_TRIANGLES];
  int n_clipped_tris = 0;

  RAL_POLY2TRI(&polygon, clipped_tris, &n_clipped_tris);
  RAL_F xs = context->width * RAL_ONE / 2;
  RAL_F ys = context->height * RAL_ONE / 2;

  for (int i = 0; i < n_clipped_tris; ++i) {
    RAL_TRI_TEX triangle = clipped_tris[i];
    for (int j = 0; j < 3; j++) {
      triangle.p[j] = RAL_M4_MXV(context->proj, triangle.p[j]);
      if (triangle.p[j].w != 0) {
        RAL_F invZ = RAL_FDIV(RAL_ONE, triangle.p[j].w);
        triangle.p[j].x = RAL_FMUL(triangle.p[j].x, invZ);
        triangle.p[j].y = RAL_FMUL(triangle.p[j].y, invZ);
        triangle.p[j].z = RAL_FMUL(triangle.p[j].z, invZ);
      }

      triangle.p[j].x = RAL_FMUL(triangle.p[j].x, xs);
      triangle.p[j].y = RAL_FMUL(-triangle.p[j].y, ys);

      triangle.p[j].x += xs;
      triangle.p[j].y += ys;
    }
    triangle.p[0].x = RAL_CLAMP(triangle.p[0].x, 0, context->width * RAL_ONE);
    triangle.p[0].y = RAL_CLAMP(triangle.p[0].y, 0, context->height * RAL_ONE);
    triangle.p[1].x = RAL_CLAMP(triangle.p[1].x, 0, context->width * RAL_ONE);
    triangle.p[1].y = RAL_CLAMP(triangle.p[1].y, 0, context->height * RAL_ONE);
    triangle.p[2].x = RAL_CLAMP(triangle.p[2].x, 0, context->width * RAL_ONE);
    triangle.p[2].y = RAL_CLAMP(triangle.p[2].y, 0, context->height * RAL_ONE);

    RAL_RASTERIZE_TEX(context, triangle.p[0].x, triangle.p[0].y,
                      triangle.p[0].w, triangle.p[1].x, triangle.p[1].y,
                      triangle.p[1].w, triangle.p[2].x, triangle.p[2].y,
                      triangle.p[2].w, triangle.uv[0].x, triangle.uv[0].y,
                      triangle.uv[1].x, triangle.uv[1].y, triangle.uv[2].x,
                      triangle.uv[2].y, texture);
  }
}

void RAL_TRIANGLE_TEX_OBJ(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az,
                      RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy,
                      RAL_F cz, RAL_F au, RAL_F av, RAL_F bu, RAL_F bv,
                      RAL_F cu, RAL_F cv, RAL_TEXTURE* texture, uint8_t obj) {
  RAL_TRI_TEX t = (RAL_TRI_TEX){
      {{ax, ay, az, RAL_ONE}, {bx, by, bz, RAL_ONE}, {cx, cy, cz, RAL_ONE}},
      {{au, av}, {bu, bv}, {cu, cv}}};

  t.p[0] = RAL_M4_MXV(context->model, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->model, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->model, t.p[2]);

  t.p[0] = RAL_M4_MXV(context->view, t.p[0]);
  t.p[1] = RAL_M4_MXV(context->view, t.p[1]);
  t.p[2] = RAL_M4_MXV(context->view, t.p[2]);
  if (t.p[0].z > context->far || t.p[1].z > context->far ||
      t.p[2].z > context->far)
    return;

  RAL_POLYGON polygon =
      RAL_TRI2POLY(t.p[0], t.p[1], t.p[2], t.uv[0], t.uv[1], t.uv[2]);

  RAL_DO_CLIP(context, &polygon);

  RAL_TRI_TEX clipped_tris[RAL_MAX_NUM_POLYGON_TRIANGLES];
  int n_clipped_tris = 0;

  RAL_POLY2TRI(&polygon, clipped_tris, &n_clipped_tris);
  RAL_F xs = context->width * RAL_ONE / 2;
  RAL_F ys = context->height * RAL_ONE / 2;

  for (int i = 0; i < n_clipped_tris; ++i) {
    RAL_TRI_TEX triangle = clipped_tris[i];
    for (int j = 0; j < 3; j++) {
      triangle.p[j] = RAL_M4_MXV(context->proj, triangle.p[j]);
      if (triangle.p[j].w != 0) {
        RAL_F invZ = RAL_FDIV(RAL_ONE, triangle.p[j].w);
        triangle.p[j].x = RAL_FMUL(triangle.p[j].x, invZ);
        triangle.p[j].y = RAL_FMUL(triangle.p[j].y, invZ);
        triangle.p[j].z = RAL_FMUL(triangle.p[j].z, invZ);
      }

      triangle.p[j].x = RAL_FMUL(triangle.p[j].x, xs);
      triangle.p[j].y = RAL_FMUL(-triangle.p[j].y, ys);

      triangle.p[j].x += xs;
      triangle.p[j].y += ys;
    }
    triangle.p[0].x = RAL_CLAMP(triangle.p[0].x, 0, context->width * RAL_ONE);
    triangle.p[0].y = RAL_CLAMP(triangle.p[0].y, 0, context->height * RAL_ONE);
    triangle.p[1].x = RAL_CLAMP(triangle.p[1].x, 0, context->width * RAL_ONE);
    triangle.p[1].y = RAL_CLAMP(triangle.p[1].y, 0, context->height * RAL_ONE);
    triangle.p[2].x = RAL_CLAMP(triangle.p[2].x, 0, context->width * RAL_ONE);
    triangle.p[2].y = RAL_CLAMP(triangle.p[2].y, 0, context->height * RAL_ONE);

    RAL_RASTERIZE_TEX_OBJ(context, triangle.p[0].x, triangle.p[0].y,
                      triangle.p[0].w, triangle.p[1].x, triangle.p[1].y,
                      triangle.p[1].w, triangle.p[2].x, triangle.p[2].y,
                      triangle.p[2].w, triangle.uv[0].x, triangle.uv[0].y,
                      triangle.uv[1].x, triangle.uv[1].y, triangle.uv[2].x,
                      triangle.uv[2].y, texture, obj);
  }
}

void RAL_RESET(RAL_CONTEXT* context) { context->model = RAL_M4_IDENT(); }
void RAL_ROTX(RAL_CONTEXT* context, RAL_F angle) {
  context->model = RAL_M4_MXM(context->model, RAL_M4_ROTX(angle));
}
void RAL_ROTY(RAL_CONTEXT* context, RAL_F angle) {
  context->model = RAL_M4_MXM(context->model, RAL_M4_ROTY(angle));
}
void RAL_ROTZ(RAL_CONTEXT* context, RAL_F angle) {
  context->model = RAL_M4_MXM(context->model, RAL_M4_ROTZ(angle));
}
void RAL_TRANSLATE(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z) {
  context->model = RAL_M4_MXM(context->model, RAL_M4_TRANS(x, y, z));
}
void RAL_SCALE(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z) {
  context->model = RAL_M4_MXM(context->model, RAL_M4_SCALE(x, y, z));
}
void RAL_SET_FOV(RAL_CONTEXT* context, RAL_F fov_in_degrees) {
  context->fov = fov_in_degrees;
  RAL_F aspect = RAL_FDIV(RAL_I2F(context->width), RAL_I2F(context->height));
  context->proj =
      RAL_M4_PROJ(fov_in_degrees, aspect, context->near, context->far);
  
  #ifdef RAL_SET_FOV_WITH_DOUBLES  
  double fov_y = RAL_F2D(fov_in_degrees) * 3.1415926 / 180.0;
  double fov_x = 2 * atan(tan(fov_y / 2.0) * RAL_F2D(aspect));
  RAL_F sx = RAL_D2F(sin(fov_x / 2.0));
  RAL_F cx = RAL_D2F(cos(fov_x / 2.0));
  RAL_F sy = RAL_D2F(sin(fov_y / 2.0));
  RAL_F cy = RAL_D2F(cos(fov_y / 2.0));
  #else
  RAL_F fov_y = RAL_FMUL(fov_in_degrees, RAL_PI) / 180;
  RAL_F fov_x = 2 * RAL_FATAN_HP(RAL_FMUL(RAL_FTAN(fov_y / 2), aspect));
  RAL_F sx = RAL_FSIN(fov_x / 2);
  RAL_F cx = RAL_FCOS(fov_x / 2);
  RAL_F sy = RAL_FSIN(fov_y / 2);
  RAL_F cy = RAL_FCOS(fov_y / 2);
  #endif
  
  //TODO: Write high precision solution that will work
  /*RAL_F fov_y = RAL_FMUL(fov_in_degrees, RAL_PI) / 180;
  RAL_F fov_x = 2 * RAL_FATAN_HP(RAL_FMUL(RAL_FTAN_HP(fov_y / 2), aspect));
  RAL_F sx = RAL_FSIN_HP(fov_x / 2);
  RAL_F cx = RAL_FCOS_HP(fov_x / 2);
  RAL_F sy = RAL_FSIN_HP(fov_y / 2);
  RAL_F cy = RAL_FCOS_HP(fov_y / 2);*/
  
  
  RAL_V4 origin = {0, 0, 0};

  context->clip_planes[RAL_LEFT_FRUSTUM_PLANE].p = origin;
  context->clip_planes[RAL_LEFT_FRUSTUM_PLANE].n = (RAL_V4){cx, 0, sx};

  context->clip_planes[RAL_RIGHT_FRUSTUM_PLANE].p = origin;
  context->clip_planes[RAL_RIGHT_FRUSTUM_PLANE].n = (RAL_V4){-cx, 0, sx};

  context->clip_planes[RAL_TOP_FRUSTUM_PLANE].p = origin;
  context->clip_planes[RAL_TOP_FRUSTUM_PLANE].n = (RAL_V4){0, -cy, sy};

  context->clip_planes[RAL_BOTTOM_FRUSTUM_PLANE].p = origin;
  context->clip_planes[RAL_BOTTOM_FRUSTUM_PLANE].n = (RAL_V4){0, cy, sy};

  context->clip_planes[RAL_NEAR_FRUSTUM_PLANE].p = (RAL_V4){0, 0, context->near};
  context->clip_planes[RAL_NEAR_FRUSTUM_PLANE].n = (RAL_V4){0, 0, RAL_ONE};

  context->clip_planes[RAL_FAR_FRUSTUM_PLANE].p = (RAL_V4){0, 0, context->far};
  context->clip_planes[RAL_FAR_FRUSTUM_PLANE].n = (RAL_V4){0, 0, -RAL_ONE};
}

void RAL_SET_CAMERA(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z, RAL_F yaw,
                    RAL_F pitch, RAL_F roll) {
  context->camera = (RAL_V4){x, y, z, RAL_ONE};
  RAL_V4 up = {0, RAL_ONE, 0, RAL_ONE};
  RAL_V4 target = {0, 0, RAL_ONE, RAL_ONE};
  up = RAL_M4_MXV(RAL_M4_ROTZ(roll), up);
  target = RAL_M4_MXV(RAL_M4_ROTX(pitch), target);
  target = RAL_M4_MXV(RAL_M4_ROTY(yaw), target);
  target = RAL_V4_ADD(context->camera, target);
  context->view = RAL_M4_QINV(RAL_POINTAT(context->camera, target, up));
}

void RAL_LOOKAT(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z) {
  RAL_V4 up = {0, RAL_ONE, 0, RAL_ONE};
  context->view =
      RAL_M4_QINV(RAL_POINTAT(context->camera, (RAL_V4){x, y, z, RAL_ONE}, up));
}

int RAL_TO_SCREEN_NC(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z, int* sx,
                     int* sy, RAL_F* sz) {
  RAL_V4 p = {x, y, z, RAL_ONE};
  p = RAL_M4_MXV(context->model, p);
  p = RAL_M4_MXV(context->view, p);
  p = RAL_M4_MXV(context->proj, p);
  if (p.w < 0) return 0;  // behind camera
  // if (p.z > context->far) return 0;

  p = RAL_V4_DIVF(p, p.w);
  RAL_F mid_x = RAL_FDIV(context->width, 2);
  RAL_F mid_y = RAL_FDIV(context->height, 2);
  p.x = RAL_FMUL((p.x + RAL_ONE), mid_x);
  p.y = RAL_FMUL((-p.y + RAL_ONE), mid_y);
  // could be off screen, but the value is still usable (clamp or clip as
  // needed)
  *sx = RAL_F2I((p.x + RAL_ONE / 2));
  *sy = RAL_F2I((p.y + RAL_ONE / 2));
  *sz = p.z;
  return 1;
}

int RAL_TO_SCREEN(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z, int* sx,
                  int* sy, RAL_F* sz) {
  RAL_V4 p = {x, y, z, RAL_ONE};
  // p = RAL_M4_MXV(context->model, p);
  p = RAL_M4_MXV(context->view, p);
  p = RAL_M4_MXV(context->proj, p);
  if (p.w < 0) return 0;  // behind camera
  if (p.z > context->far) return 0;

  p = RAL_V4_DIVF(p, p.w);
  RAL_F mid_x = RAL_FDIV(context->width, 2);
  RAL_F mid_y = RAL_FDIV(context->height, 2);
  p.x = RAL_FMUL((p.x + RAL_ONE), mid_x);
  p.y = RAL_FMUL((-p.y + RAL_ONE), mid_y);
  // could be off screen, but the value is still usable (clamp or clip as
  // needed)
  *sx = RAL_F2I((p.x + RAL_ONE / 2));
  *sy = RAL_F2I((p.y + RAL_ONE / 2));
  *sz = p.z;
  return 1;
}

int RAL_TO_SCREEN_M(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z, int* sx,
                    int* sy, RAL_F* sz) {
  RAL_V4 p = {x, y, z, RAL_ONE};
  p = RAL_M4_MXV(context->model, p);
  p = RAL_M4_MXV(context->view, p);
  p = RAL_M4_MXV(context->proj, p);
  if (p.w < 0) return 0;  // behind camera
  if (p.z > context->far) return 0;

  p = RAL_V4_DIVF(p, p.w);
  RAL_F mid_x = RAL_FDIV(context->width, 2);
  RAL_F mid_y = RAL_FDIV(context->height, 2);
  p.x = RAL_FMUL((p.x + RAL_ONE), mid_x);
  p.y = RAL_FMUL((-p.y + RAL_ONE), mid_y);
  // could be off screen, but the value is still usable (clamp or clip as
  // needed)
  *sx = RAL_F2I((p.x + RAL_ONE / 2));
  *sy = RAL_F2I((p.y + RAL_ONE / 2));
  *sz = p.z;
  return 1;
}

void RAL_CLEAR(RAL_CONTEXT* context) {
  memset(context->depth, 0x7f,
         context->width * context->height * (int)sizeof(context->depth[0]));
  memset(context->pixels, 0,
         context->width * context->height * (int)sizeof(context->pixels[0]));
  memset(
      context->object_buf, 255,
      context->width * context->height * (int)sizeof(context->object_buf[0]));
}

void RAL_CLEAR_DEPTH(RAL_CONTEXT* context) {
  memset(context->depth, 0x7f,
         context->width * context->height * (int)sizeof(context->depth[0]));
}

void RAL_INIT(RAL_CONTEXT* context, uint32_t* pixel_buffer, RAL_F* depth_buffer,
              uint8_t* obj_buffer, int w, int h, RAL_F fov) {
  context->width = w;
  context->height = h;
  context->pixels = pixel_buffer;
  context->depth = depth_buffer;
  context->object_buf = obj_buffer;
  RAL_CLEAR(context);
  RAL_RESET(context);
  RAL_SET_FOV(context, fov);

  RAL_SET_CAMERA(context, 0, 0, 0, 0, 0, 0);
}