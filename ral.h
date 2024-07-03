/*
	RAL - Render ALready, a fixed point software renderer.
	Basically a port of BOOTLEG3D by Benedict Henshaw to a fixed point rendering.
	by Ilya Efimov, 2024.
	CC0 1.0 Universal, Public Domain.
*/

#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define RAL_CLAMP(X, lo, hi) (X<lo ? lo : X>hi ? hi : X)
#define RAL_MIN(a, b) ((a) > (b) ? (b) : (a))
#define RAL_MAX(a, b) ((a) < (b) ? (b) : (a))
#define RAL_ABS(a) ((a) > 0 ? (a) : -(a))

typedef int64_t RAL_F;

typedef struct { RAL_F x, y, z, w; } RAL_V4;
typedef struct { RAL_F m[4][4]; } RAL_M4;
typedef struct { RAL_V4 p[3]; } RAL_TRI;


#define RAL_FP_SCALE 16
#define RAL_FP_UNIT (1 << RAL_FP_SCALE)
#define RAL_SHIFT_MASK ((1 << RAL_FP_SCALE) - 1) 
#define RAL_ONE (1 << RAL_FP_SCALE)

#define RAL_I2F(a) ((a) << RAL_FP_SCALE)
#define RAL_F2I(a) ((a) >> RAL_FP_SCALE)

#define RAL_D2F(a) ((RAL_F)(a * (double)RAL_FP_UNIT))
#define RAL_F2D(a) ((double)(a) / (double)RAL_FP_UNIT)

#define RAL_FMUL(a, b) (RAL_F)(((int64_t)a * (int64_t)b) >> RAL_FP_SCALE)
#define RAL_FDIV(a, b) (b == 0 ? 0 : (RAL_F)(((int64_t)(a) << RAL_FP_SCALE) / (int64_t)(b)))

#define RAL_FLOOR(a) RAL_I2F(RAL_F2I((a)))

#define RAL_PI RAL_FDIV(355 * RAL_ONE, 112 * RAL_ONE)


typedef struct {
	int width, height;
	uint32_t * pixels;
	RAL_F * depth;
	RAL_M4 model, view, proj;
	RAL_V4 camera;
	RAL_F near, far;
} RAL_CONTEXT;

RAL_F RAL_FSIN(RAL_F x){
  int sign = 1;
    
  if (x < 0) // odd function
  {
    x *= -1;
    sign = -1;
  }
    
  x %= 2 * RAL_PI;
  
  if (x > RAL_PI)
  {
    x -= RAL_PI;
    sign *= -1;
  }
    
  x *= RAL_PI - x;
  
  return sign * (16 * x) / ((5 * RAL_PI * RAL_PI - 4 * x) / RAL_ONE);

}

RAL_F RAL_FCOS(RAL_F x){
	return RAL_FSIN(x + (RAL_PI >> 1));
}

RAL_F RAL_FTAN(RAL_F x){
	return RAL_FDIV(RAL_FSIN(x), RAL_FCOS(x));
}

RAL_F RAL_SQRT(RAL_F A)
{
	int invert = 0;
	int iter = RAL_FP_SCALE;
	int l, i;

	if (A < 0)
		return (-1);
	if (A == 0 || A == RAL_ONE)
		return (A);
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
	for (i = 0; i < iter; i++)
		l = (l + RAL_FDIV(A, l)) >> 1;
	if (invert)
		return (RAL_FDIV(RAL_ONE, l));
	return (l);

}

RAL_V4 RAL_V4_ADD(RAL_V4 a, RAL_V4 b){
	RAL_V4 r;
	r.x = a.x + b.x;
	r.y = a.y + b.y;
	r.z = a.z + b.z;
	r.w = a.w + b.w;
	return r;
}

RAL_V4 RAL_V4_SUB(RAL_V4 a, RAL_V4 b){
	RAL_V4 r;
	r.x = a.x - b.x;
	r.y = a.y - b.y;
	r.z = a.z - b.z;
	r.w = a.w - b.w;
	return r;
}

RAL_V4 RAL_V4_MUL(RAL_V4 a, RAL_V4 b){
	RAL_V4 r;
	r.x = RAL_FMUL(a.x, b.x);
	r.y = RAL_FMUL(a.y, b.y);
	r.z = RAL_FMUL(a.z, b.z);
	r.w = RAL_FMUL(a.w, b.w);
	return r;
}

RAL_V4 RAL_V4_MULF(RAL_V4 a, RAL_F b){
	RAL_V4 r;
	r.x = RAL_FMUL(a.x, b);
	r.y = RAL_FMUL(a.y, b);
	r.z = RAL_FMUL(a.z, b);
	r.w = RAL_FMUL(a.w, b);
	return r;
}

RAL_V4 RAL_V4_DIVF(RAL_V4 a, RAL_F b){
	RAL_V4 r;
	r.x = RAL_FDIV(a.x, b);
	r.y = RAL_FDIV(a.y, b);
	r.z = RAL_FDIV(a.z, b);
	r.w = RAL_FDIV(a.w, b);
	return r;
}


RAL_V4 RAL_V4_NORMALIZE(RAL_V4 a){
	RAL_F norm = RAL_SQRT(RAL_FMUL(a.x, a.x) + RAL_FMUL(a.y, a.y) + RAL_FMUL(a.z, a.z));
	if(norm == 0) return a;
	return RAL_V4_DIVF(a, norm);
}
RAL_F RAL_V4_DOT(RAL_V4 a, RAL_V4 b){
	RAL_F r = RAL_FMUL(a.x, b.x) + RAL_FMUL(a.y, b.y) + RAL_FMUL(a.z, b.z);
	return r;
}

RAL_V4 RAL_V4_CROSS(RAL_V4 a, RAL_V4 b)
{
	RAL_V4 r;
	r.x = RAL_FMUL(a.y, b.z) - RAL_FMUL(a.z, b.y);
	r.y = RAL_FMUL(a.z, b.x) - RAL_FMUL(a.x, b.z);
	r.z = RAL_FMUL(a.x, b.y) - RAL_FMUL(a.y, b.x);
	r.w = 1;
	return r;
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
    fov = RAL_FDIV(RAL_ONE, RAL_FTAN(RAL_FMUL(fov, RAL_FMUL(RAL_FDIV((RAL_ONE / 2), 180 * RAL_ONE), RAL_PI))));
    return (RAL_M4){{
        [0][0] = RAL_FMUL(aspect, fov),
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
                RAL_FMUL(a.m[r][0], b.m[0][c]) +
                RAL_FMUL(a.m[r][1], b.m[1][c]) +
                RAL_FMUL(a.m[r][2], b.m[2][c]) +
                RAL_FMUL(a.m[r][3], b.m[3][c]);
        }
    }
    return matrix;
}

RAL_V4 RAL_M4_MXV(RAL_M4 m, RAL_V4 v) {
    return (RAL_V4){
        RAL_FMUL(v.x, m.m[0][0]) + RAL_FMUL(v.y, m.m[1][0]) + RAL_FMUL(v.z, m.m[2][0]) + RAL_FMUL(v.w, m.m[3][0]),
        RAL_FMUL(v.x, m.m[0][1]) + RAL_FMUL(v.y, m.m[1][1]) + RAL_FMUL(v.z, m.m[2][1]) + RAL_FMUL(v.w, m.m[3][1]),
        RAL_FMUL(v.x, m.m[0][2]) + RAL_FMUL(v.y, m.m[1][2]) + RAL_FMUL(v.z, m.m[2][2]) + RAL_FMUL(v.w, m.m[3][2]),
        RAL_FMUL(v.x, m.m[0][3]) + RAL_FMUL(v.y, m.m[1][3]) + RAL_FMUL(v.z, m.m[2][3]) + RAL_FMUL(v.w, m.m[3][3]),
    };
}

RAL_M4 RAL_M4_QINV(RAL_M4 m) {
    RAL_M4 o = (RAL_M4){{
        [0][0] = m.m[0][0], [0][1] = m.m[1][0], [0][2] = m.m[2][0], [0][3] = 0,
        [1][0] = m.m[0][1], [1][1] = m.m[1][1], [1][2] = m.m[2][1], [1][3] = 0,
        [2][0] = m.m[0][2], [2][1] = m.m[1][2], [2][2] = m.m[2][2], [2][3] = 0,
    }};
    o.m[3][0] = -(RAL_FMUL(m.m[3][0], o.m[0][0]) + RAL_FMUL(m.m[3][1], o.m[1][0]) + RAL_FMUL(m.m[3][2], o.m[2][0]));
    o.m[3][1] = -(RAL_FMUL(m.m[3][0], o.m[0][1]) + RAL_FMUL(m.m[3][1], o.m[1][1]) + RAL_FMUL(m.m[3][2], o.m[2][1]));
    o.m[3][2] = -(RAL_FMUL(m.m[3][0], o.m[0][2]) + RAL_FMUL(m.m[3][1], o.m[1][2]) + RAL_FMUL(m.m[3][2], o.m[2][2]));
    o.m[3][3] = RAL_ONE;
    return o;
}

RAL_M4 RAL_POINTAT(RAL_V4 pos, RAL_V4 target, RAL_V4 up) {
    RAL_V4 forward = RAL_V4_SUB(target, pos);
    forward = RAL_V4_NORMALIZE(forward);
    RAL_V4 a = RAL_V4_MULF(forward, RAL_V4_DOT(up, forward));
    up = RAL_V4_NORMALIZE(RAL_V4_SUB(up, a));
    RAL_V4 right = RAL_V4_CROSS(up, forward);
    return (RAL_M4){{
        [0][0] = right.x,   [0][1] = right.y,   [0][2] = right.z,   [0][3] = 0,
        [1][0] = up.x,      [1][1] = up.y,      [1][2] = up.z,      [1][3] = 0,
        [2][0] = forward.x, [2][1] = forward.y, [2][2] = forward.z, [2][3] = 0,
        [3][0] = pos.x,     [3][1] = pos.y,     [3][2] = pos.z,     [3][3] = RAL_ONE,
    }};
}

RAL_V4 RAL_INTERSECT_PLANE(RAL_V4 plane, RAL_V4 norm, RAL_V4 start, RAL_V4 end) {
    norm = RAL_V4_NORMALIZE(norm);
    RAL_F delta = -RAL_V4_DOT(norm, plane);
    RAL_F ad = RAL_V4_DOT(start, norm);
    RAL_F bd = RAL_V4_DOT(end, norm);
    RAL_F t = RAL_FDIV((-delta - ad), (bd - ad));
    RAL_V4 start_to_end = RAL_V4_SUB(end, start);
    RAL_V4 segment = RAL_V4_MULF(start_to_end, t);
    return RAL_V4_ADD(start, segment);
}

int RAL_CLIP_PLANE(RAL_V4 plane, RAL_V4 norm, RAL_TRI in, RAL_TRI out[2]) {
    norm = RAL_V4_NORMALIZE(norm);
    RAL_V4* inside[3];
    int inside_count = 0;
    RAL_V4* outside[3];
    int outside_count = 0;
    RAL_F d0 = (RAL_FMUL(norm.x, in.p[0].x) + RAL_FMUL(norm.y, in.p[0].y) + RAL_FMUL(norm.z, in.p[0].z) - RAL_V4_DOT(norm, plane));
    RAL_F d1 = (RAL_FMUL(norm.x, in.p[1].x) + RAL_FMUL(norm.y, in.p[1].y) + RAL_FMUL(norm.z, in.p[1].z) - RAL_V4_DOT(norm, plane));
    RAL_F d2 = (RAL_FMUL(norm.x, in.p[2].x) + RAL_FMUL(norm.y, in.p[2].y) + RAL_FMUL(norm.z, in.p[2].z) - RAL_V4_DOT(norm, plane));
	
    if (d0 >= 0) inside[inside_count++] = &in.p[0]; else outside[outside_count++] = &in.p[0];
    if (d1 >= 0) inside[inside_count++] = &in.p[1]; else outside[outside_count++] = &in.p[1];
    if (d2 >= 0) inside[inside_count++] = &in.p[2]; else outside[outside_count++] = &in.p[2];
    if (inside_count == 3) {
        out[0] = in;
        return 1;
    } else if (inside_count == 1 && outside_count == 2) {
        out[0].p[0] = *inside[0];
        out[0].p[1] = RAL_INTERSECT_PLANE(plane, norm, *inside[0], *outside[0]);
        out[0].p[2] = RAL_INTERSECT_PLANE(plane, norm, *inside[0], *outside[1]);
        return 1;
    } else if (inside_count == 2 && outside_count == 1) {
        out[0].p[0] = *inside[0];
        out[0].p[1] = *inside[1];
        out[0].p[2] = RAL_INTERSECT_PLANE(plane, norm, *inside[0], *outside[0]);
        out[1].p[0] = *inside[1];
        out[1].p[1] = out[0].p[2];
        out[1].p[2] = RAL_INTERSECT_PLANE(plane, norm, *inside[1], *outside[0]);
        return 2;
    }
    return 0;
}

void RAL_RASTERIZE(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az, RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy, RAL_F cz, uint32_t c) {
    ax = RAL_FLOOR(ax); bx = RAL_FLOOR(bx); cx = RAL_FLOOR(cx);
    ay = RAL_FLOOR(ay); by = RAL_FLOOR(by); cy = RAL_FLOOR(cy);
    RAL_F t = 0;
    if (ay > by) { t = ax; ax = bx; bx = t; t = ay; ay = by; by = t; t = az; az = bz; bz = t; }
    if (ay > cy) { t = ax; ax = cx; cx = t; t = ay; ay = cy; cy = t; t = az; az = cz; cz = t; }
    if (by > cy) { t = bx; bx = cx; cx = t; t = by; by = cy; cy = t; t = bz; bz = cz; cz = t; }
    RAL_F alpha = 0, alpha_step = RAL_FDIV(RAL_ONE, (cy - ay));
    RAL_F beta  = 0, beta_step  = RAL_FDIV(RAL_ONE, (by - ay));
    for (int y = RAL_F2I(ay); y < RAL_F2I(by); y++) {
        RAL_F sx = ax + RAL_FMUL((cx - ax), alpha);
        RAL_F sz = az + RAL_FMUL((cz - az), alpha);
        RAL_F ex = ax + RAL_FMUL((bx - ax), beta);
        RAL_F ez = az + RAL_FMUL((bz - az), beta);
        if (sx > ex) { t = sx; sx = ex; ex = t; t = sz; sz = ez; ez = t; }
        RAL_F depth_step = RAL_FDIV((ez - sz), (ex - sx));
        RAL_F d = sz;
        int end = RAL_F2I(ex);

        for (int x = RAL_F2I(sx); x < end; ++x) {
            int p = x + y * context->width;
            if (p >= 0 && p < context->width * context->height && d < context->depth[p]) { context->depth[p] = d; context->pixels[p] = c; }
            d += depth_step;
        }
        alpha += alpha_step;
        beta += beta_step;
    }
    beta = 0;
    beta_step = RAL_FDIV(RAL_ONE, (cy - by));
    for (int y = RAL_F2I(by); y < RAL_F2I(cy); y++) {
        RAL_F sx = ax + RAL_FMUL((cx - ax), alpha);
        RAL_F sz = az + RAL_FMUL((cz - az), alpha);
        RAL_F ex = bx + RAL_FMUL((cx - bx), beta);
        RAL_F ez = bz + RAL_FMUL((cz - bz), beta);
        if (sx > ex) { t = sx; sx = ex; ex = t; t = sz; sz = ez; ez = t; }
        RAL_F depth_step = RAL_FDIV((ez - sz), (ex - sx));
        RAL_F d = sz;
        int end = RAL_F2I(ex);
        for (int x = RAL_F2I(sx); x < end; ++x) {
            int p = x + y * context->width;
            if (p >= 0 && p < context->width * context->height && d < context->depth[p]) { context->depth[p] = d; context->pixels[p] = c; }
            d += depth_step;
        }
        alpha += alpha_step;
        beta += beta_step;
    }
}

int RAL_TRIANGLE(RAL_CONTEXT* context, RAL_F ax, RAL_F ay, RAL_F az, RAL_F bx, RAL_F by, RAL_F bz, RAL_F cx, RAL_F cy, RAL_F cz, uint32_t c) {
    RAL_TRI t = (RAL_TRI){{{ax,ay,az,RAL_ONE},{bx,by,bz,RAL_ONE},{cx,cy,cz,RAL_ONE}}};
    t.p[0] = RAL_M4_MXV(context->model, t.p[0]);
    t.p[1] = RAL_M4_MXV(context->model, t.p[1]);
    t.p[2] = RAL_M4_MXV(context->model, t.p[2]);
	
    RAL_V4 line_a = RAL_V4_SUB(t.p[1], t.p[0]);
    RAL_V4 line_b = RAL_V4_SUB(t.p[2], t.p[0]);
    RAL_V4 normal = RAL_V4_CROSS(line_a, line_b);
    RAL_V4 cam_ray = RAL_V4_SUB(t.p[0], context->camera);
    if (RAL_V4_DOT(normal, cam_ray) > RAL_ONE / 100) return 0;

    t.p[0] = RAL_M4_MXV(context->view, t.p[0]);
    t.p[1] = RAL_M4_MXV(context->view, t.p[1]);
    t.p[2] = RAL_M4_MXV(context->view, t.p[2]);
	
    RAL_TRI clipped[2];
    int count = RAL_CLIP_PLANE(
        (RAL_V4){ 0, 0, context->near, RAL_ONE },
        (RAL_V4){ 0, 0, RAL_ONE, RAL_ONE },
        t,
        clipped
    );

    if (count == 0) return 0;
    RAL_TRI queue[16];
    int queue_count = 0;
    for (int n = 0; n < count; ++n) {
        t.p[0] = RAL_M4_MXV(context->proj, clipped[n].p[0]);
        t.p[1] = RAL_M4_MXV(context->proj, clipped[n].p[1]);
        t.p[2] = RAL_M4_MXV(context->proj, clipped[n].p[2]);
        t.p[0] = RAL_V4_DIVF(t.p[0], t.p[0].w);
        t.p[1] = RAL_V4_DIVF(t.p[1], t.p[1].w);
        t.p[2] = RAL_V4_DIVF(t.p[2], t.p[2].w);
        RAL_F xs = RAL_FDIV(context->width, 2);
        RAL_F ys = RAL_FDIV(context->height, 2);
        t.p[0].x = RAL_FMUL(( t.p[0].x + RAL_ONE), xs);
        t.p[0].y = RAL_FMUL((-t.p[0].y + RAL_ONE), ys);
        t.p[1].x = RAL_FMUL(( t.p[1].x + RAL_ONE), xs);
        t.p[1].y = RAL_FMUL((-t.p[1].y + RAL_ONE), ys);
        t.p[2].x = RAL_FMUL(( t.p[2].x + RAL_ONE), xs);
        t.p[2].y = RAL_FMUL((-t.p[2].y + RAL_ONE), ys);
        queue[queue_count++] = t;
    }
    RAL_V4 tp = { 0, RAL_ONE / 2, 0, RAL_ONE };
    RAL_V4 tn = { 0, RAL_ONE, 0, RAL_ONE };
    RAL_V4 bp = { 0, RAL_I2F(context->height), 0, RAL_ONE };
    RAL_V4 bn = { 0, -RAL_ONE, 0, RAL_ONE };
    RAL_V4 lp = { RAL_ONE / 2, 0, 0, RAL_ONE };
    RAL_V4 ln = { RAL_ONE, 0, 0, RAL_ONE };
    RAL_V4 rp = { RAL_I2F(context->width), 0, 0, RAL_ONE };
    RAL_V4 rn = { -RAL_ONE, 0, 0, RAL_ONE };
	
    int triangles_to_clip = queue_count;
    for (int p = 0; p < 4; ++p) {
        int n = 0;
        while (triangles_to_clip > 0) {
            RAL_TRI test = queue[0];
            --queue_count;
            --triangles_to_clip;
            memmove(queue, queue + 1, (int)sizeof(RAL_TRI) * queue_count);
            switch (p) {
                case 0: n = RAL_CLIP_PLANE(tp, tn, test, clipped); break;
                case 1: n = RAL_CLIP_PLANE(bp, bn, test, clipped); break;
                case 2: n = RAL_CLIP_PLANE(lp, ln, test, clipped); break;
                case 3: n = RAL_CLIP_PLANE(rp, rn, test, clipped); break;
            }

            for (int w = 0; w < n; ++w) {
                queue[queue_count++] = clipped[w];
            }
        }
        triangles_to_clip = queue_count;
    }

    for (int i = 0; i < queue_count; ++i) {
        RAL_TRI * triangle = &queue[i];
        RAL_RASTERIZE(
			context,
            triangle->p[0].x, triangle->p[0].y, triangle->p[0].z,
            triangle->p[1].x, triangle->p[1].y, triangle->p[1].z,
            triangle->p[2].x, triangle->p[2].y, triangle->p[2].z,
            c
        );
    }
    return 1;
}

void RAL_RESET(RAL_CONTEXT* context) { context->model = RAL_M4_IDENT(); }
void RAL_ROTX(RAL_CONTEXT* context, RAL_F angle) { context->model = RAL_M4_MXM(context->model, RAL_M4_ROTX(angle)); }
void RAL_ROTY(RAL_CONTEXT* context, RAL_F angle) { context->model = RAL_M4_MXM(context->model, RAL_M4_ROTY(angle)); }
void RAL_ROTZ(RAL_CONTEXT* context, RAL_F angle) { context->model = RAL_M4_MXM(context->model, RAL_M4_ROTZ(angle)); }
void RAL_TRANSLATE(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z) { context->model = RAL_M4_MXM(context->model, RAL_M4_TRANS(x, y, z)); }
void RAL_SCALE(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z) { context->model = RAL_M4_MXM(context->model, RAL_M4_SCALE(x, y, z)); }
void RAL_SET_FOV(RAL_CONTEXT* context, RAL_F fov_in_degrees) { context->proj = RAL_M4_PROJ(fov_in_degrees, RAL_FDIV(RAL_I2F(context->height), RAL_I2F(context->width)), context->near, context->far); }

void RAL_SET_CAMERA(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z, RAL_F yaw, RAL_F pitch, RAL_F roll) {
    context->camera = (RAL_V4){ x, y, z, RAL_ONE };
    RAL_V4 up = { 0, RAL_ONE, 0, RAL_ONE };
    RAL_V4 target = { 0, 0, RAL_ONE, RAL_ONE };
    up = RAL_M4_MXV(RAL_M4_ROTZ(roll), up);
    target = RAL_M4_MXV(RAL_M4_ROTX(pitch), target);
    target = RAL_M4_MXV(RAL_M4_ROTY(yaw), target);
    target = RAL_V4_ADD(context->camera, target);
    context->view = RAL_M4_QINV(RAL_POINTAT(context->camera, target, up));
}

void RAL_LOOKAT(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z) {
    RAL_V4 up = { 0, RAL_ONE, 0, RAL_ONE };
    context->view = RAL_M4_QINV(RAL_POINTAT(context->camera, (RAL_V4){ x, y, z, RAL_ONE }, up));
}

int RAL_TO_SCREEN(RAL_CONTEXT* context, RAL_F x, RAL_F y, RAL_F z, int * sx, int * sy) {
    RAL_V4 p = { x, y, z, RAL_ONE };
    p = RAL_M4_MXV(context->model, p);
    p = RAL_M4_MXV(context->view, p);
    p = RAL_M4_MXV(context->proj, p);
    p = RAL_V4_DIVF(p, p.w);
    if (p.w < 0) return 0; // behind camera
    p = RAL_V4_DIVF(p, p.w);
    RAL_F mid_x = RAL_FDIV(context->width, 2);
    RAL_F mid_y = RAL_FDIV(context->height, 2);
    p.x = RAL_FMUL(( p.x + RAL_ONE), mid_x);
    p.y = RAL_FMUL((-p.y + RAL_ONE), mid_y);
    // could be off screen, but the value is still usable (clamp or clip as needed)
    *sx = (int)(p.x + RAL_ONE / 2);
    *sy = (int)(p.y + RAL_ONE / 2);
    return 1;
}

void RAL_CLEAR(RAL_CONTEXT* context) {
    memset(context->depth, 0x7f, context->width * context->height * (int)sizeof(context->depth[0]));
    memset(context->pixels, 0, context->width * context->height * (int)sizeof(context->pixels[0]));
}

void RAL_INIT(RAL_CONTEXT* context, uint32_t * pixel_buffer, RAL_F * depth_buffer, int w, int h, RAL_F fov) {
    context->width = w;
    context->height = h;
    context->pixels = pixel_buffer;
    context->depth = depth_buffer;
    RAL_CLEAR(context);
    RAL_RESET(context);
    context->proj = RAL_M4_PROJ(fov, RAL_FDIV(RAL_I2F(context->height), RAL_I2F(context->width)), context->near, context->far);
    RAL_SET_CAMERA(context, 0, 0, 0, 0, 0, 0);
}

