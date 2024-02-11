# fractals-linear-algebra-and-haskell
    The main point of this repository is the little linear algebra library I wrote to do my homework which was interesting and little bit hard endeavour for me.
## How to run the functions?
  There are 2 versions of the homework. The only difference is
hw1-v1.hs includes this main function:
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/f9e0a466-9b26-402e-9610-263a16493d59)

  and hw1-v2.hs doesn’t include that. So, we can compile hw1-v1.hs and
run the hw1-v1.exe . I compiled and ran it, and as a result, wanted stl
files were generated.

If wanted, in cmd, from the interactive environment ghci we can
load hw1-v2.hs and write these functions ourselves and generate
wanted stl files.

part1, part2, part3 and part4 functions are the stl file generator
functions of the respective parts in the homework. The values given in
the main function are for example purposes. You can use these
functions yourself in the interactive environment ghci by loading
hw1-v2.hs . After compiling hw1-v1.hs, the executable file is going to run
the functions in the picture.

## Part 1 - Sierpinski’s Triangle
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/5fef0873-fc6f-452e-95b3-8344e62ec694)

The marked function is the wanted function of part 1 of the
homework. While I was doing part 1, I have not used the power of linear
algebra so my code here is pretty primitive, just a recursion and list
concatenation.


## Part 2 - Koch’s Snowflake
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/41f14a2d-6315-497c-b5c8-14effebbd376)

The marked function is the
wanted function of part 2 of the homework. With this code we can use any
triangle for Koch’s Snowflake anywhere in 3D space.

To generate the new triangles, first we need to identify their points so
we can draw them on the space. 2 points for the base is easy. Finding the
third sharp point is the hard part. For this first we find the normal of the
plane of the triangle. After that, we take the cross product of the normal of
the triangle with a vector that was generated from the triangle's two
vertices. By doing this, we find the direction of the vector which is
orthogonal to the edge of the triangle to reach the third point we seek. After
some vector summation we reach our goal.

### COMING OF THE LINEAR ALGEBRA  

  From the moment I understood the weakness of my flesh, it
disgusted me. I craved the strength and certainty of Linear Algebra. I
aspired to the purity of the Blessed Vector.

In this part, I understood I need the power of linear algebra so I wrote
some helper code to implement it. 

## Part 3 - Cube at a specified position
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/2fb09e7e-c81f-4790-8edc-d9a49b5d86f8)

  The marked function here is the wanted function of part 3 of the
homework. To create a cube we need to specify 2 vectors to describe its
orientation and 1 point to describe its place in space.(1,0,0) and (0,1,0)
are the vectors of the example cube.

shapeCube function generates the shape of the cube, createCube
specifies every aspect of the cube for the 3D space. By the way, I
created a new type just for the cube because its shape alone is not
enough to describe it.

## Part 4 - Cube Pattern
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/34784276-b94d-4fb2-9f41-d16f4915d785)

  Like before, the marked function here is the wanted function of part
4 of the homework. Like said in the code’s comments, cubb here is just a
standard cube I defined so that we could generate a pattern on the base
of it.

In all parts of the homework, recursion functions can take any
triangle or any kind of cube and generate their pattern with them. To
generate concrete examples, I defined some default cubes. For the
triangles, like asked in the homework, I used triangle t1.

## Results

Below are the resulting stl files showed in CloudCompare:

### Part 1 
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/f997912d-f142-459e-8207-170b364bc482)

### Part 2
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/003f696a-3e85-4f3f-9477-2dd8506bb471)

### Part 3 
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/846d0314-5f98-48f8-8c54-3abea14999b9)

### Part 4
![image](https://github.com/ASAlici/fractals-linear-algebra-and-haskell/assets/144834934/3108a1ca-f258-4010-b36e-2f43e152842b)










