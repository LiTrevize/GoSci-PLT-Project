float g [m][s -2] = 9.8;

struct Object {
  float mass [kg];
  float h [m];
  float v [m][s -1];
  float a [m][s -2];
}

/* Simulate the motion of an object suspended from a spring */
func simulate(Object obj, float kSpring [kg][s -2], float dt [s]) Object {
  float force [kg][m][s -2] = - obj.mass * g - obj.h * kSpring;
  obj.a = force / obj.mass;
  obj.v = obj.v + obj.a * dt;
  obj.h = obj.h + obj.v * dt;
  return obj;
}

func main() int {
  int i;
  Object obj = Object{2.0, 5.0, 0.0, 0.0};
  for (i=0;i<100;i++) {
    obj = simulate(obj, 5.0, 0.1);
    print(obj.h);
  }
  return 0;
}