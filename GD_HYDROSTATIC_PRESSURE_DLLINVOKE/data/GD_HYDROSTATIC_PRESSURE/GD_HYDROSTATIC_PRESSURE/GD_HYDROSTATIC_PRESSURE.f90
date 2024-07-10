
    SUBROUTINE GD_HYDROSTATIC_PRESSURE(A,B,C,D,E,F,&        !坝体坐标(2)
                                       WL_Up,&              !上游水位
                                       WL_Down,&            !下游水位
                                       Density,&            !水密度
                                       Gravity,&            !重力加速度
        
                                       A_F,&                !AF段静水压力(4)(大小，合力点坐标，夹角)
                                       F_E,&
                                       C_D,&
                                       B_C,&
                                       AREA_AF,&            !AF段静水压力图
                            
                                       AREA_FE,&
                                       AREA_CD,&
                                       AREA_BC)
    !DEC$ ATTRIBUTES DLLEXPORT::GD_HYDROSTATIC_PRESSURE
    implicit none
    
    !接口
    real*8 :: A(2),B(2),C(2),D(2),E(2),F(2)
    real*8 :: WL_Up,WL_Down,Density,Gravity
    real*8 :: A_F(4),F_E(4),C_D(4),B_C(4)
    real*8 :: AREA_AF(5,2),AREA_FE(5,2),AREA_CD(5,2),AREA_BC(5,2)
    
    !中间参数
    real*8 :: Load_Fill_Factor  !图缩放倍数
    real*8 :: Unit_Line(2)
    real*8 :: Cr1(3),Cr2(3)
    real*8 :: Unit_Vector3D(3),Unit_VectorFA(2),Unit_VectorEF(2),Unit_VectorBC(2),Unit_VectorCD(2)
    real*8 :: WaterHead_A,WaterHead_VectorA(2),WaterHeadPoint_A(2)
    real*8 :: WaterHead_F,WaterHead_VectorF(2),WaterHeadPoint_F(2)
    real*8 :: WaterHead_E,WaterHead_VectorE(2),WaterHeadPoint_E(2)
    real*8 :: WaterHead_B,WaterHead_VectorB(2),WaterHeadPoint_B(2)
    real*8 :: WaterHead_C,WaterHead_VectorC(2),WaterHeadPoint_C(2)
    real*8 :: WaterHead_D,WaterHead_VectorD(2),WaterHeadPoint_D(2)
    real*8 :: Points_FA(5,2),Points_EF(5,2),Points_BC(5,2),Points_CD(5,2)    !区间静水压力图的控制点
    real*8 :: WaterHeadPoint_A_Fill(2),WaterHeadPoint_F_Fill(2),WaterHeadPoint_E_Fill(2),WaterHeadPoint_B_Fill(2),WaterHeadPoint_C_Fill(2),WaterHeadPoint_D_Fill(2),Centroid_X,Centroid_Y
    real*8 :: Points_Centroid(4,2),Points_Area_Centroid,Perpendicular_Distance
    real*8 :: temp1(2),temp2(2)     !插值用
    real*8 :: Zero_WaterLevel_Point(2)  !水位与坝体交点
    real*8 :: NORM
    
    !初始化
    A_F = 0
    F_E = 0
    C_D = 0
    B_C = 0
    AREA_AF = 0
    AREA_FE = 0
    AREA_CD = 0
    AREA_BC = 0
    
    !计算主体
    Load_Fill_Factor = 50000
    
    !上游水位
    !在A点之上
    if(WL_Up>A(2)) then
        !****A-F之间的静水压力****!
        Unit_Line = (A-F)/NORM(A-F,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorFA = Unit_Vector3D(1:2)
        
        !A点静水压力和坐标
        WaterHead_A = Density*Gravity*(WL_Up-A(2))
        WaterHead_VectorA = Unit_VectorFA*WaterHead_A
        WaterHeadPoint_A = WaterHead_VectorA + A
        
        !F点静水压力和坐标
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorFA*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        Points_FA(1,:) = A
        Points_FA(2,:) = F
        Points_FA(3,:) = WaterHeadPoint_F
        Points_FA(4,:) = WaterHeadPoint_A
        Points_FA(5,:) = A
        
        !缩放出图
        WaterHeadPoint_A_Fill = WaterHead_VectorA/Load_Fill_Factor + A
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        
        AREA_AF = Points_FA
        AREA_AF(3,:) = WaterHeadPoint_F_Fill
        AREA_AF(4,:) = WaterHeadPoint_A_Fill
        
        call polyarea(5,Points_FA(:,1),Points_FA(:,2),A_F(1))
        A_F(1) = A_F(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_FA(:,1),Points_FA(:,2),Centroid_X,Centroid_Y,1)
        Points_Centroid(1,:) = A
        Points_Centroid(2,:) = F
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = A
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(A-F,2)
        A_F(2) = Centroid_X - Perpendicular_Distance*Unit_VectorFA(1)
        A_F(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorFA(2)
        A_F(4) = atan2d(-Unit_VectorFA(2),-Unit_VectorFA(1))
        
        !****F-E之间的静水压力****!
        Unit_Line = (F-E)/NORM(F-E,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorEF = Unit_Vector3D(1:2)
        
        !F点静水压力和坐标
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorEF*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        !E点静水压力和坐标
        WaterHead_E = Density*Gravity*(WL_Up-E(2))
        WaterHead_VectorE = Unit_VectorEF*WaterHead_E
        WaterHeadPoint_E = WaterHead_VectorE + E
        
        Points_EF(1,:) = F
        Points_EF(2,:) = E
        Points_EF(3,:) = WaterHeadPoint_E
        Points_EF(4,:) = WaterHeadPoint_F
        Points_EF(5,:) = F
        
        !缩放出图
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        WaterHeadPoint_E_Fill = WaterHead_VectorE/Load_Fill_Factor + E
        
        AREA_FE = Points_EF
        AREA_FE(3,:) = WaterHeadPoint_E_Fill
        AREA_FE(4,:) = WaterHeadPoint_F_Fill
        
        call polyarea(5,Points_EF(:,1),Points_EF(:,2),F_E(1))
        F_E(1) = F_E(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_EF(:,1),Points_EF(:,2),Centroid_X,Centroid_Y,1)
        Points_Centroid(1,:) = E
        Points_Centroid(2,:) = F
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = E
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(E-F,2)
        F_E(2) = Centroid_X - Perpendicular_Distance*Unit_VectorEF(1)
        F_E(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorEF(2)
        F_E(4) = atan2d(-Unit_VectorEF(2),-Unit_VectorEF(1))
    end if
    
    !在A-F之间
    if(F(2)<WL_Up.and.WL_Up<=A(2)) then
        !****A-F之间的静水压力****!
        Unit_Line = (A-F)/NORM(A-F,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorFA = Unit_Vector3D(1:2)
        
        temp1(1) = A(2)
        temp1(2) = F(2)
        temp2(1) = A(1)
        temp2(2) = F(1)
        call INTERPOLATION(2,temp1,temp2,WL_Up,Zero_WaterLevel_Point(1))
        Zero_WaterLevel_Point(2) = WL_Up
        
        !A点静水压力和坐标
        WaterHead_A = Density*Gravity*(WL_Up-Zero_WaterLevel_Point(2))
        WaterHead_VectorA = Unit_VectorFA*WaterHead_A
        WaterHeadPoint_A = WaterHead_VectorA + Zero_WaterLevel_Point
        
        !F点静水压力和坐标
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorFA*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        Points_FA(1,:) = Zero_WaterLevel_Point
        Points_FA(2,:) = F
        Points_FA(3,:) = WaterHeadPoint_F
        Points_FA(4,:) = WaterHeadPoint_A
        Points_FA(5,:) = Zero_WaterLevel_Point
        
        !缩放出图
        WaterHeadPoint_A_Fill = WaterHead_VectorA/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        
        AREA_AF = Points_FA
        AREA_AF(3,:) = WaterHeadPoint_F_Fill
        AREA_AF(4,:) = WaterHeadPoint_A_Fill
        
        call polyarea(5,Points_FA(:,1),Points_FA(:,2),A_F(1))
        A_F(1) = A_F(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_FA(:,1),Points_FA(:,2),Centroid_X,Centroid_Y,1)
        Points_Centroid(1,:) = A
        Points_Centroid(2,:) = F
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = A
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(A-F,2)
        A_F(2) = Centroid_X - Perpendicular_Distance*Unit_VectorFA(1)
        A_F(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorFA(2)
        A_F(4) = atan2d(-Unit_VectorFA(2),-Unit_VectorFA(1))
        
        !****F-E之间的静水压力****!
        Unit_Line = (F-E)/NORM(F-E,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorEF = Unit_Vector3D(1:2)
        
        !F点静水压力和坐标
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorEF*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        !E点静水压力和坐标
        WaterHead_E = Density*Gravity*(WL_Up-E(2))
        WaterHead_VectorE = Unit_VectorEF*WaterHead_E
        WaterHeadPoint_E = WaterHead_VectorE + E
        
        Points_EF(1,:) = F
        Points_EF(2,:) = E
        Points_EF(3,:) = WaterHeadPoint_E
        Points_EF(4,:) = WaterHeadPoint_F
        Points_EF(5,:) = F
        
        !缩放出图
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        WaterHeadPoint_E_Fill = WaterHead_VectorE/Load_Fill_Factor + E
        
        AREA_FE = Points_EF
        AREA_FE(3,:) = WaterHeadPoint_E_Fill
        AREA_FE(4,:) = WaterHeadPoint_F_Fill
        
        call polyarea(5,Points_EF(:,1),Points_EF(:,2),F_E(1))
        F_E(1) = F_E(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_EF(:,1),Points_EF(:,2),Centroid_X,Centroid_Y,1)
        Points_Centroid(1,:) = E
        Points_Centroid(2,:) = F
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = E
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(E-F,2)
        F_E(2) = Centroid_X - Perpendicular_Distance*Unit_VectorEF(1)
        F_E(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorEF(2)
        F_E(4) = atan2d(-Unit_VectorEF(2),-Unit_VectorEF(1))
    end if
    
    !在F-E之间
    if(E(2)<WL_Up.and.WL_Up<=F(2)) then
        Unit_Line = (F-E)/NORM(F-E,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorEF = Unit_Vector3D(1:2)
        
        temp1(1) = E(2)
        temp1(2) = F(2)
        temp2(1) = E(1)
        temp2(2) = F(1)
        call INTERPOLATION(2,temp1,temp2,WL_Up,Zero_WaterLevel_Point(1))
        Zero_WaterLevel_Point(2) = WL_Up
        
        !F点静水压力和坐标
        WaterHead_F = Density*Gravity*(WL_Up-Zero_WaterLevel_Point(2))
        WaterHead_VectorF = Unit_VectorEF*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + Zero_WaterLevel_Point
        
        !E点静水压力和坐标
        WaterHead_E = Density*Gravity*(WL_Up-E(2))
        WaterHead_VectorE = Unit_VectorEF*WaterHead_E
        WaterHeadPoint_E = WaterHead_VectorE + E
        
        Points_EF(1,:) = Zero_WaterLevel_Point
        Points_EF(2,:) = E
        Points_EF(3,:) = WaterHeadPoint_E
        Points_EF(4,:) = WaterHeadPoint_F
        Points_EF(5,:) = Zero_WaterLevel_Point
        
        !缩放出图
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_E_Fill = WaterHead_VectorE/Load_Fill_Factor + E
        
        AREA_FE = Points_EF
        AREA_FE(3,:) = WaterHeadPoint_E_Fill
        AREA_FE(4,:) = WaterHeadPoint_F_Fill
        
        call polyarea(5,Points_EF(:,1),Points_EF(:,2),F_E(1))
        F_E(1) = F_E(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_EF(:,1),Points_EF(:,2),Centroid_X,Centroid_Y,1)
        Points_Centroid(1,:) = E
        Points_Centroid(2,:) = F
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = E
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(E-F,2)
        F_E(2) = Centroid_X - Perpendicular_Distance*Unit_VectorEF(1)
        F_E(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorEF(2)
        F_E(4) = atan2d(-Unit_VectorEF(2),-Unit_VectorEF(1))
    end if
    
    !下游水位
    !在B点之上
    if(WL_Down>B(2)) then
        !****B-C之间的静水压力****!
        Unit_Line = (B-C)/NORM(B-C,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorBC = Unit_Vector3D(1:2)
        
        !B点静水压力和坐标
        WaterHead_B = Density*Gravity*(WL_Down-B(2))
        WaterHead_VectorB = Unit_VectorBC*WaterHead_B
        WaterHeadPoint_B = WaterHead_VectorB + B
        
        !C点静水压力和坐标
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorBC*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        Points_BC(1,:) = B
        Points_BC(2,:) = C
        Points_BC(3,:) = WaterHeadPoint_C
        Points_BC(4,:) = WaterHeadPoint_B
        Points_BC(5,:) = B
        
        !缩放出图
        WaterHeadPoint_B_Fill = WaterHead_VectorB/Load_Fill_Factor + B
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        
        AREA_BC = Points_BC
        AREA_BC(3,:) = WaterHeadPoint_C_Fill
        AREA_BC(4,:) = WaterHeadPoint_B_Fill
        
        call polyarea(5,Points_BC(:,1),Points_BC(:,2),B_C(1))
        B_C(1) = B_C(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_BC(:,1),Points_BC(:,2),Centroid_X,Centroid_Y,2)
        Points_Centroid(1,:) = B
        Points_Centroid(2,:) = C
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = B
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(B-C,2)
        B_C(2) = Centroid_X - Perpendicular_Distance*Unit_VectorBC(1)
        B_C(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorBC(2)
        B_C(4) = atan2d(-Unit_VectorBC(2),-Unit_VectorBC(1))
        
        !****C-D之间的静水压力****!
        Unit_Line = (C-D)/NORM(C-D,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorCD = Unit_Vector3D(1:2)
        
        !C点静水压力和坐标
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorCD*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        !D点静水压力和坐标
        WaterHead_D = Density*Gravity*(WL_Down-D(2))
        WaterHead_VectorD = Unit_VectorCD*WaterHead_D
        WaterHeadPoint_D = WaterHead_VectorD + D
        
        Points_CD(1,:) = C
        Points_CD(2,:) = D
        Points_CD(3,:) = WaterHeadPoint_D
        Points_CD(4,:) = WaterHeadPoint_C
        Points_CD(5,:) = C
        
        !缩放出图
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        WaterHeadPoint_D_Fill = WaterHead_VectorD/Load_Fill_Factor + D
        
        AREA_CD = Points_CD
        AREA_CD(3,:) = WaterHeadPoint_D_Fill
        AREA_CD(4,:) = WaterHeadPoint_C_Fill
        
        call polyarea(5,Points_CD(:,1),Points_CD(:,2),C_D(1))
        C_D(1) = C_D(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_CD(:,1),Points_CD(:,2),Centroid_X,Centroid_Y,2)
        Points_Centroid(1,:) = C
        Points_Centroid(2,:) = D
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = C
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(D-C,2)
        C_D(2) = Centroid_X - Perpendicular_Distance*Unit_VectorCD(1)
        C_D(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorCD(2)
        C_D(4) = atan2d(-Unit_VectorCD(2),-Unit_VectorCD(1))
    end if
    
    !在B-C之间
    if(C(2)<WL_Down.and.WL_Down<=B(2)) then
        !****B-C之间的静水压力****!
        Unit_Line = (B-C)/NORM(B-C,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorBC = Unit_Vector3D(1:2)
        
        temp1(1) = B(2)
        temp1(2) = C(2)
        temp2(1) = B(1)
        temp2(2) = C(1)
        call INTERPOLATION(2,temp1,temp2,WL_Down,Zero_WaterLevel_Point(1))
        Zero_WaterLevel_Point(2) = WL_Down
        
        !A点静水压力和坐标
        WaterHead_B = Density*Gravity*(WL_Down-Zero_WaterLevel_Point(2))
        WaterHead_VectorB = Unit_VectorBC*WaterHead_B
        WaterHeadPoint_B = WaterHead_VectorB + Zero_WaterLevel_Point
        
        !F点静水压力和坐标
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorBC*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        Points_BC(1,:) = Zero_WaterLevel_Point
        Points_BC(2,:) = C
        Points_BC(3,:) = WaterHeadPoint_C
        Points_BC(4,:) = WaterHeadPoint_B
        Points_BC(5,:) = Zero_WaterLevel_Point
        
        !缩放出图
        WaterHeadPoint_B_Fill = WaterHead_VectorB/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        
        AREA_BC = Points_BC
        AREA_BC(3,:) = WaterHeadPoint_C_Fill
        AREA_BC(4,:) = WaterHeadPoint_B_Fill
        
        call polyarea(5,Points_BC(:,1),Points_BC(:,2),B_C(1))
        B_C(1) = B_C(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_BC(:,1),Points_BC(:,2),Centroid_X,Centroid_Y,2)
        Points_Centroid(1,:) = B
        Points_Centroid(2,:) = C
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = B
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(B-C,2)
        B_C(2) = Centroid_X - Perpendicular_Distance*Unit_VectorBC(1)
        B_C(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorBC(2)
        B_C(4) = atan2d(-Unit_VectorBC(2),-Unit_VectorBC(1))
        
        !****C-D之间的静水压力****!
        Unit_Line = (C-D)/NORM(C-D,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorCD = Unit_Vector3D(1:2)
        
        !C点静水压力和坐标
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorCD*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        !D点静水压力和坐标
        WaterHead_D = Density*Gravity*(WL_Down-D(2))
        WaterHead_VectorD = Unit_VectorCD*WaterHead_D
        WaterHeadPoint_D = WaterHead_VectorD + D
        
        Points_CD(1,:) = C
        Points_CD(2,:) = D
        Points_CD(3,:) = WaterHeadPoint_D
        Points_CD(4,:) = WaterHeadPoint_C
        Points_CD(5,:) = C
        
        !缩放出图
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        WaterHeadPoint_D_Fill = WaterHead_VectorD/Load_Fill_Factor + D
        
        AREA_CD = Points_CD
        AREA_CD(3,:) = WaterHeadPoint_D_Fill
        AREA_CD(4,:) = WaterHeadPoint_C_Fill
        
        call polyarea(5,Points_CD(:,1),Points_CD(:,2),C_D(1))
        C_D(1) = C_D(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_CD(:,1),Points_CD(:,2),Centroid_X,Centroid_Y,2)
        Points_Centroid(1,:) = C
        Points_Centroid(2,:) = D
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = C
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(D-C,2)
        C_D(2) = Centroid_X - Perpendicular_Distance*Unit_VectorCD(1)
        C_D(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorCD(2)
        C_D(4) = atan2d(-Unit_VectorCD(2),-Unit_VectorCD(1))
    end if
    
    !在C-D之间
    if(D(2)<WL_Down.and.WL_Down<=C(2)) then
        Unit_Line = (C-D)/NORM(C-D,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorCD = Unit_Vector3D(1:2)
        
        temp1(1) = C(2)
        temp1(2) = D(2)
        temp2(1) = C(1)
        temp2(2) = D(1)
        call INTERPOLATION(2,temp1,temp2,WL_Down,Zero_WaterLevel_Point(1))
        Zero_WaterLevel_Point(2) = WL_Down
        
        !C点静水压力和坐标
        WaterHead_C = Density*Gravity*(WL_Down-Zero_WaterLevel_Point(2))
        WaterHead_VectorC = Unit_VectorCD*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + Zero_WaterLevel_Point
        
        !E点静水压力和坐标
        WaterHead_D = Density*Gravity*(WL_Down-D(2))
        WaterHead_VectorD = Unit_VectorCD*WaterHead_D
        WaterHeadPoint_D = WaterHead_VectorD + D
        
        Points_CD(1,:) = Zero_WaterLevel_Point
        Points_CD(2,:) = D
        Points_CD(3,:) = WaterHeadPoint_D
        Points_CD(4,:) = WaterHeadPoint_C
        Points_CD(5,:) = Zero_WaterLevel_Point
        
        !缩放出图
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_D_Fill = WaterHead_VectorD/Load_Fill_Factor + D
        
        AREA_CD = Points_CD
        AREA_CD(3,:) = WaterHeadPoint_D_Fill
        AREA_CD(4,:) = WaterHeadPoint_C_Fill
        
        call polyarea(5,Points_CD(:,1),Points_CD(:,2),C_D(1))
        C_D(1) = C_D(1)/1000.0      !合力大小化为KN
        
        !将形心投影到作用边界上
        call centroid(5,Points_CD(:,1),Points_CD(:,2),Centroid_X,Centroid_Y,2)
        Points_Centroid(1,:) = C
        Points_Centroid(2,:) = D
        Points_Centroid(3,1) = Centroid_X
        Points_Centroid(3,2) = Centroid_Y
        Points_Centroid(4,:) = C
        call polyarea(4,Points_Centroid(:,1),Points_Centroid(:,2),Points_Area_Centroid)
        Perpendicular_Distance = 2.0*Points_Area_Centroid/NORM(D-C,2)
        C_D(2) = Centroid_X - Perpendicular_Distance*Unit_VectorCD(1)
        C_D(3) = Centroid_Y - Perpendicular_Distance*Unit_VectorCD(2)
        C_D(4) = atan2d(-Unit_VectorCD(2),-Unit_VectorCD(1))
    end if
    
    end subroutine
        
        
