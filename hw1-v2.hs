


type Point = (Float, Float, Float)
type Triangle = ( Point ,Point ,Point )
type Shape = [ Triangle ]

t1::Triangle
t1 = ((0 ,0 ,10) ,(0 , -5 ,0) ,(0 ,5 ,0) )

t2::Triangle
t2 = ((0 ,0 ,20) ,(0 , -5 ,10) ,(0 ,5 ,10) )

t3::Triangle
t3 = ((0 ,0 ,30) ,(0 , -5 ,20) ,(0 ,5 ,20) )

createTriangleDef::Triangle->String
createTriangleDef ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3))="  facet\n" ++ 
                                                     "      outer loop\n" ++
                                                     "          vertex " ++
                                                     (show x1) ++ " " ++ (show y1) ++ " " ++ (show z1) ++ "\n" ++
                                                     "          vertex " ++
                                                     (show x2) ++ " " ++ (show y2) ++ " " ++ (show z2) ++ "\n" ++
                                                     "          vertex " ++
                                                     (show x3) ++ " " ++ (show y3) ++ " " ++ (show z3) ++ "\n" ++
                                                     "      endloop\n" ++
                                                     "  endfacet\n"

createObjectModelString::Shape->String
createObjectModelString n = "solid Object01\n" ++ concat [createTriangleDef y|y<-n ] ++ " endsolid Object01"

writeObjModel::Shape -> String -> IO ()

writeObjModel x filename = do writeFile filename (createObjectModelString x)






-- Yardimci fonksiyonlar --------------------------------------------------------------------


eskenar256::Triangle
eskenar256 = ((128 ,0 ,0) ,(-128 , 0 ,0) ,(0, 128 * sqrt 3,0) )


first :: (a, b, c) -> a
first (a,b,c) = a

second :: (a, b, c) -> b
second (a,b,c) = b

third :: (a, b, c) -> c
third (a,b,c) = c

middle::Point->Point->Point
middle (x1,y1,z1) (x2,y2,z2) = ((x1+x2)/2 , (y1+y2)/2 , (z1+z2)/2 )


type Vector = (Float,Float,Float)

vectorLength::Vector->Float
vectorLength (v1,v2,v3) = sqrt (v1^2 + v2^2 + v3^2)

distance::Point->Point->Float
distance (x1,y1,z1) (x2,y2,z2) = sqrt ((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)

vectorSum::Vector->Vector->Vector
vectorSum (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

pointVectorSum::Point->Vector->Point
pointVectorSum (p1,p2,p3) (v1,v2,v3) = (p1+v1,p2+v2,p3+v3)

scalarProduct::Float->Vector->Vector
scalarProduct k (x,y,z) = (k*x,k*y,k*z)


vectorNormalization::Vector->Vector
vectorNormalization v = scalarProduct (1/(vectorLength v)) v

makeVector::Point->Point->Vector
makeVector (a1,a2,a3) (b1,b2,b3) = (b1-a1,b2-a2,b3-a3)

makeUnitVector::Point->Point->Vector
makeUnitVector (a1,a2,a3) (b1,b2,b3) = vectorNormalization (makeVector (a1,a2,a3) (b1,b2,b3))

normalOfPlane::Point->Point->Point->Vector
normalOfPlane p1 p2 p3 = vectorNormalization (crossProduct (makeVector p1 p2) (makeVector p1 p3))


crossProduct::Vector->Vector->Vector
crossProduct (a1,a2,a3) (b1,b2,b3) = (  a2*b3-a3*b2  ,  -(a1*b3-a3*b1)  ,  a1*b2-a2*b1  ) 

dotProduct::Vector->Vector->Float
dotProduct (a1,a2,a3) (b1,b2,b3) = a1*b1+a2*b2+a3*b3

reverseVector::Vector->Vector
reverseVector (v1,v2,v3) = (-v1,-v2,-v3)

--2 vektor arasindaki acinin cos'u ve sin'i
coss::Vector->Vector->Float
coss v1 v2 =  (dotProduct v1 v2) / (vectorLength v1 * vectorLength v2)

sinn::Vector->Vector->Float
sinn v1 v2 = sqrt (1 - (coss v1 v2)^2)


--yonunu belirlemek icin 2 yon vektorunu, merkezini ve kenarinin uzunlugunu girerek bir yuzey yarat
makeSurface::Vector->Vector->Point->Float->Shape
makeSurface n1 n2 p k = [tri1,tri2]
    where
        unit1 = vectorNormalization n1
        unit2 = vectorNormalization n2
        tri1 = ( pointVectorSum p (vectorSum (scalarProduct (k/2) unit1) (scalarProduct (k/2) unit2)), pointVectorSum p (vectorSum (scalarProduct (-k/2) unit1) (scalarProduct (-k/2) unit2)), pointVectorSum p (vectorSum (scalarProduct (k/2) unit1) (scalarProduct (-k/2) unit2)) )
        tri2 = ( pointVectorSum p (vectorSum (scalarProduct (k/2) unit1) (scalarProduct (k/2) unit2)), pointVectorSum p (vectorSum (scalarProduct (-k/2) unit1) (scalarProduct (-k/2) unit2)), pointVectorSum p (vectorSum (scalarProduct (-k/2) unit1) (scalarProduct (k/2) unit2)) )


surfaceTest :: IO ()
surfaceTest = writeObjModel (makeSurface (1,0,0) (0,1,0) (0,0,0) 10) "testsurface.stl" 



-- Yardimci fonksiyonlar ---------------------------------------------------------------------------------------





--PART 1 ------------------------------------------------------------------------------------------------------

sierpin::Int->Triangle->Shape
sierpin h tri 
    | h==0 = [tri]
    | otherwise = (sierpin (h-1) tri1) ++ (sierpin (h-1) tri2) ++ (sierpin (h-1) tri3)
        where
            tri1 = (first tri , middle (first tri) (second tri), middle (first tri) (third tri))
            tri2 = (middle (first tri) (second tri) , second tri , middle (second tri) (third tri))
            tri3 = ( middle (first tri) (third tri) , middle (third tri) (second tri) , third tri)


sierpinski :: Int -> Shape
sierpinski n = sierpin n t1

part1 :: Int -> IO ()
part1 n = writeObjModel (sierpinski n) "part1.stl"


--PART 1 ------------------------------------------------------------------------------------------------------



--PART 2 ------------------------------------------------------------------------------------------------------


koch::Int->Triangle->Shape
koch h tri 
    | h==0 = [tri]
    | otherwise = [tri] ++ (koch (h-1) tri1) ++ (koch (h-1) tri2) ++ (koch (h-1) tri3) ++ (koch (h-1) tri4) ++ (koch (h-1) tri5) ++ (koch (h-1) tri6)
        where
            tri1 =  (tr1P1,tr1P2,tr1P3)
            tri2 =  (tr2P1,tr2P2,tr2P3)
            tri3 =  (tr3P1,tr3P2,tr3P3)
            
            tri4 = (p1,tr1P1,tr3P3)
            tri5 = (tr1P3,p2,tr2P1)
            tri6 = (tr3P1,tr2P3,p3)

            p1 = first tri
            p2 = second tri
            p3 = third tri
            cos1 = coss (makeVector p1 p2) (makeVector p1 p3)
            cos2 = coss (makeVector p2 p3) (makeVector p2 p1)
            cos3 = coss (makeVector p3 p1) (makeVector p3 p2)

            sin1 = sinn (makeVector p1 p2) (makeVector p1 p3)
            sin2 = sinn (makeVector p2 p3) (makeVector p2 p1)
            sin3 = sinn (makeVector p3 p1) (makeVector p3 p2)




            h12 =  scalarProduct ((sin1/3) * vectorLength (makeVector p1 p2))           (vectorNormalization (crossProduct (makeVector p1 p2) normal))
            v12 =  scalarProduct (cos1/3)  (makeVector p1 p2)
            k12 =  vectorSum h12 v12
            tr1P2 = pointVectorSum  tr1P1  k12
            tr1P1 = pointVectorSum p1 (scalarProduct (1/3) (makeVector p1 p2))
            tr1P3 = pointVectorSum p1 (scalarProduct (2/3) (makeVector p1 p2)) 

            h23 =  scalarProduct (  (sin1/3) * vectorLength (makeVector p1 p2)  )       (vectorNormalization (crossProduct (makeVector p2 p3) normal))
            v23 =  scalarProduct (  (cos1/3) * vectorLength (makeVector p1 p2)  )       (makeUnitVector p2 p3)
            k23 =  vectorSum h23 v23
            tr2P2 = pointVectorSum  tr2P1  k23
            tr2P1 = pointVectorSum p2 (scalarProduct (1/3) (makeVector p2 p3))
            tr2P3 = pointVectorSum p2 (scalarProduct (2/3) (makeVector p2 p3))

            h31 =  scalarProduct (  (sin1/3) * vectorLength (makeVector p1 p2)  )       (vectorNormalization (crossProduct (makeVector p3 p1) normal))
            v31 =  scalarProduct (  (cos1/3) * vectorLength (makeVector p1 p2)  )       (makeUnitVector p3 p1)
            k31 =  vectorSum h31 v31
            tr3P2 = pointVectorSum  tr3P1  k31
            tr3P1 = pointVectorSum p3 (scalarProduct (1/3) (makeVector p3 p1))
            tr3P3 = pointVectorSum p3 (scalarProduct (2/3) (makeVector p3 p1))  





            normal = normalOfPlane p1 p2 p3


--part2'de istenilen fonksiyon sekline getirdim, yukaridaki fonksiyon spesifik olarak t1 ucgenine uygulanıyor.
kochSnowflake :: Int -> Shape
kochSnowflake n = koch n t1

part2 :: Int -> IO ()
part2 n = writeObjModel (kochSnowflake n) "part2.stl"

--PART 2 ------------------------------------------------------------------------------------------------------



--PART 3 ------------------------------------------------------------------------------------------------------

shapeCube::Vector->Vector->Point->Float->Shape
shapeCube n1 n2 p k = surface1++surface2++surface3++surface4++surface5++surface6
    where
        unit1 = vectorNormalization n1
        unit2 = vectorNormalization n2
        surface1 = makeSurface unit1 unit2 (    pointVectorSum p ( scalarProduct (k/2) (vectorNormalization (crossProduct unit1 unit2))    )        ) k
        
        surface2Unit1 = unit1
        surface2Unit2 = reverseVector (vectorNormalization (crossProduct unit1 unit2) )
        surface2 = makeSurface surface2Unit1 surface2Unit2 (pointVectorSum p (scalarProduct (k/2) unit2)    )  k

        surface3 = makeSurface unit1 unit2 (    pointVectorSum p (reverseVector ( scalarProduct (k/2) (vectorNormalization (crossProduct unit1 unit2))))) k      

        surface4Unit1 = unit1
        surface4Unit2 = reverseVector (vectorNormalization (crossProduct unit1 unit2) )
        surface4 = makeSurface surface4Unit1 surface4Unit2 (pointVectorSum p ( reverseVector (scalarProduct (k/2) unit2))    )  k

        surface5Unit1 = reverseVector (vectorNormalization (crossProduct unit1 unit2) )
        surface5Unit2 = unit2
        surface5 = makeSurface surface5Unit1 surface5Unit2 (pointVectorSum p (scalarProduct (k/2) unit1))  k

        surface6Unit1 = reverseVector (vectorNormalization (crossProduct unit1 unit2) )
        surface6Unit2 = unit2
        surface6 = makeSurface surface6Unit1 surface6Unit2 (pointVectorSum p ( reverseVector (scalarProduct (k/2) unit1))    )  k

testCube :: IO ()
testCube = writeObjModel (shapeCube (1,0,0) (0,1,0) (0,0,0) 10) "testcube.stl"


--part3'te ornek olarak kullanmak uzere olusturulmus cube shape'i
exampleCubeShape::Point->Float->Shape
exampleCubeShape p k = shapeCube (1,0,0) (0,1,0) p k


type Cube = (Vector,Vector,Point,Float,Shape)
createCube::Vector->Vector->Point->Float->Cube
createCube n1 n2 p k = (n1,n2,p,k,shapeCube n1 n2 p k)


part3 :: Point -> Float -> IO ()
part3 p k = writeObjModel (exampleCubeShape p k) "part3.stl"

--PART 3 ------------------------------------------------------------------------------------------------------



--PART 4 ------------------------------------------------------------------------------------------------------

cubePatternRecur::Int->Cube->Shape
cubePatternRecur h cub 
    | h==0 = shape
    | otherwise = shape ++ (cubePatternRecur (h-1) cub1) ++ (cubePatternRecur (h-1) cub2) ++ (cubePatternRecur (h-1) cub3) ++ (cubePatternRecur (h-1) cub4) ++ (cubePatternRecur (h-1) cub5) ++ (cubePatternRecur (h-1) cub6)
        where
            (n1_,n2_,p,k,shape)=cub
            n1=vectorNormalization n1_
            n2=vectorNormalization n2_
            cub1 = createCube n1 n2 (pointVectorSum p (scalarProduct (3*k/4) (vectorNormalization (crossProduct n1 n2))) ) (k/2)
            cub2 = createCube n1 n2 (pointVectorSum p (scalarProduct (3*k/4) (reverseVector (vectorNormalization (crossProduct n1 n2))) )) (k/2)
            cub3 = createCube n1 n2 (pointVectorSum p (scalarProduct (3*k/4) n1) ) (k/2)
            cub4 = createCube n1 n2 (pointVectorSum p (scalarProduct (3*k/4) (reverseVector n1)) ) (k/2)
            cub5 = createCube n1 n2 (pointVectorSum p (scalarProduct (3*k/4) n2) ) (k/2)
            cub6 = createCube n1 n2 (pointVectorSum p (scalarProduct (3*k/4) (reverseVector n2)) ) (k/2)
            

--part4'te kullanilmak uzere olusturulmus ornek cube
cubb = createCube (1,0,0) (0,1,0) (0,0,0) 16

--cubePatternRecur'u ornekte istenilen fonksiyon'a donusturdum.
cubePattern n = cubePatternRecur n cubb

part4 n = writeObjModel (cubePattern n ) "part4.stl"

--PART 4 ------------------------------------------------------------------------------------------------------




