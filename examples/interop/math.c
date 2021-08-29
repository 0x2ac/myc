int add(int x, int y) { return x + y; }

typedef struct {
    double x;
    double y;
    double z;
} Vec3;

void sumVec3(Vec3 left, Vec3 right, Vec3* result) {
    result->x = left.x + right.x;
    result->y = left.y + right.y;
    result->z = left.z + right.z;
}
