float mapX(float x) 
{
  return x*3.25 - 2;
}

float mapY(float y) 
{
  return y*2.5 - 1.25;
}

int index(int x, int y, int width) 
{
  return x*4+4*width*y;
}

char mynormalize(char v)
{
   if (v>255) return 0;
   return v;
}

__kernel void render(__global char * output) 
{
  const int kmax = 256;
  int iterLimit = 100;
  float qmin = -1.5;
  float qmax = 1.5;
  float pmin = -2.25;
  float pmax = 0.75;

  float p = 0.0;
  float q = 0.0;
  float k = 0.0;
  float x0 = 0.0;
  float y0 = 0.0;
  float x = 0.0;
  float y = 0.0;
  float r = 0.0;

  int x_dim = get_global_id(0);
  int y_dim = get_global_id(1);
  size_t width = get_global_size(0);
  size_t height = get_global_size(1);
  
  float xstep = (pmax - pmin) / width;
  float ystep = (qmax - qmin) / height;

  int sx = x_dim;
  int sy = y_dim;
  {  
    {    
         p = pmin + xstep * sx;
         q = qmax - ystep * sy;
         k = 0;
         x0 = 0;
         y0 = 0;

         do {
            x = x0 * x0 - y0 * y0 + p;
            y = 2 * x0 * y0 + q;
            x0 = x;
            y0 = y;
            r = x * x + y * y;
            k++;
		 }
         while ((r <= iterLimit) && (k < kmax));

         if (k >= kmax) {
   		   k = 0;
		 }

      int idx = index(sx, sy, width);
      output[idx] = mynormalize(k*5*x*y/100);
      output[idx + 1] = mynormalize(k*x/10);
      output[idx + 2] = mynormalize(k*y/10);
      output[idx + 3] = 255;
		 
    }
  }
  
 
}

__kernel void render_fixed(__global char * output) 
{
  const int kmax = 256;
  int iterLimit = 100;
  float qmin = -1.5;
  float qmax = 1.5;
  float pmin = -2.25;
  float pmax = 0.75;

  float p = 0.0;
  float q = 0.0;
  float k = 0.0;
  float x0 = 0.0;
  float y0 = 0.0;
  float x = 0.0;
  float y = 0.0;
  float r = 0.0;

  int x_dim = get_global_id(0);
  int y_dim = get_global_id(1);
  //size_t width = get_global_size(0);
  //size_t height = get_global_size(1);
  //size_t width  = 200;      //note: can take up 12s! (500x500) 
  //size_t height = 200;
  size_t width  = 1024;      //note: can take up 12s! (500x500) 
  size_t height = 1024;
  
  float xstep = (pmax - pmin) / width;
  float ystep = (qmax - qmin) / height;

  for (int sx = 0; sx < width; ++sx)      
  {  
    for (int sy = 0; sy < height; ++sy) 
    {    
         p = pmin + xstep * sx;
         q = qmax - ystep * sy;
         k = 0;
         x0 = 0;
         y0 = 0;

         do {
            x = x0 * x0 - y0 * y0 + p;
            y = 2 * x0 * y0 + q;
            x0 = x;
            y0 = y;
            r = x * x + y * y;
            k++;
		 }
         while ((r <= iterLimit) && (k < kmax));

         if (k >= kmax) {
   		   k = 0;
		 }

      int idx = index(sx, sy, width);
      output[idx] = mynormalize(k*5*x*y/100);
      output[idx + 1] = mynormalize(k*x/10);
      output[idx + 2] = mynormalize(k*y/10);
      output[idx + 3] = 255;
		 
    }
  }
  
 
}