
    SUBROUTINE GD_HYDROSTATIC_PRESSURE(A,B,C,D,E,F,&        !��������(2)
                                       WL_Up,&              !����ˮλ
                                       WL_Down,&            !����ˮλ
                                       Density,&            !ˮ�ܶ�
                                       Gravity,&            !�������ٶ�
        
                                       A_F,&                !AF�ξ�ˮѹ��(4)(��С�����������꣬�н�)
                                       F_E,&
                                       C_D,&
                                       B_C,&
                                       AREA_AF,&            !AF�ξ�ˮѹ��ͼ
                            
                                       AREA_FE,&
                                       AREA_CD,&
                                       AREA_BC)
    !DEC$ ATTRIBUTES DLLEXPORT::GD_HYDROSTATIC_PRESSURE
    implicit none
    
    !�ӿ�
    real*8 :: A(2),B(2),C(2),D(2),E(2),F(2)
    real*8 :: WL_Up,WL_Down,Density,Gravity
    real*8 :: A_F(4),F_E(4),C_D(4),B_C(4)
    real*8 :: AREA_AF(5,2),AREA_FE(5,2),AREA_CD(5,2),AREA_BC(5,2)
    
    !�м����
    real*8 :: Load_Fill_Factor  !ͼ���ű���
    real*8 :: Unit_Line(2)
    real*8 :: Cr1(3),Cr2(3)
    real*8 :: Unit_Vector3D(3),Unit_VectorFA(2),Unit_VectorEF(2),Unit_VectorBC(2),Unit_VectorCD(2)
    real*8 :: WaterHead_A,WaterHead_VectorA(2),WaterHeadPoint_A(2)
    real*8 :: WaterHead_F,WaterHead_VectorF(2),WaterHeadPoint_F(2)
    real*8 :: WaterHead_E,WaterHead_VectorE(2),WaterHeadPoint_E(2)
    real*8 :: WaterHead_B,WaterHead_VectorB(2),WaterHeadPoint_B(2)
    real*8 :: WaterHead_C,WaterHead_VectorC(2),WaterHeadPoint_C(2)
    real*8 :: WaterHead_D,WaterHead_VectorD(2),WaterHeadPoint_D(2)
    real*8 :: Points_FA(5,2),Points_EF(5,2),Points_BC(5,2),Points_CD(5,2)    !���侲ˮѹ��ͼ�Ŀ��Ƶ�
    real*8 :: WaterHeadPoint_A_Fill(2),WaterHeadPoint_F_Fill(2),WaterHeadPoint_E_Fill(2),WaterHeadPoint_B_Fill(2),WaterHeadPoint_C_Fill(2),WaterHeadPoint_D_Fill(2),Centroid_X,Centroid_Y
    real*8 :: Points_Centroid(4,2),Points_Area_Centroid,Perpendicular_Distance
    real*8 :: temp1(2),temp2(2)     !��ֵ��
    real*8 :: Zero_WaterLevel_Point(2)  !ˮλ����彻��
    real*8 :: NORM
    
    !��ʼ��
    A_F = 0
    F_E = 0
    C_D = 0
    B_C = 0
    AREA_AF = 0
    AREA_FE = 0
    AREA_CD = 0
    AREA_BC = 0
    
    !��������
    Load_Fill_Factor = 50000
    
    !����ˮλ
    !��A��֮��
    if(WL_Up>A(2)) then
        !****A-F֮��ľ�ˮѹ��****!
        Unit_Line = (A-F)/NORM(A-F,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorFA = Unit_Vector3D(1:2)
        
        !A�㾲ˮѹ��������
        WaterHead_A = Density*Gravity*(WL_Up-A(2))
        WaterHead_VectorA = Unit_VectorFA*WaterHead_A
        WaterHeadPoint_A = WaterHead_VectorA + A
        
        !F�㾲ˮѹ��������
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorFA*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        Points_FA(1,:) = A
        Points_FA(2,:) = F
        Points_FA(3,:) = WaterHeadPoint_F
        Points_FA(4,:) = WaterHeadPoint_A
        Points_FA(5,:) = A
        
        !���ų�ͼ
        WaterHeadPoint_A_Fill = WaterHead_VectorA/Load_Fill_Factor + A
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        
        AREA_AF = Points_FA
        AREA_AF(3,:) = WaterHeadPoint_F_Fill
        AREA_AF(4,:) = WaterHeadPoint_A_Fill
        
        call polyarea(5,Points_FA(:,1),Points_FA(:,2),A_F(1))
        A_F(1) = A_F(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
        
        !****F-E֮��ľ�ˮѹ��****!
        Unit_Line = (F-E)/NORM(F-E,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorEF = Unit_Vector3D(1:2)
        
        !F�㾲ˮѹ��������
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorEF*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        !E�㾲ˮѹ��������
        WaterHead_E = Density*Gravity*(WL_Up-E(2))
        WaterHead_VectorE = Unit_VectorEF*WaterHead_E
        WaterHeadPoint_E = WaterHead_VectorE + E
        
        Points_EF(1,:) = F
        Points_EF(2,:) = E
        Points_EF(3,:) = WaterHeadPoint_E
        Points_EF(4,:) = WaterHeadPoint_F
        Points_EF(5,:) = F
        
        !���ų�ͼ
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        WaterHeadPoint_E_Fill = WaterHead_VectorE/Load_Fill_Factor + E
        
        AREA_FE = Points_EF
        AREA_FE(3,:) = WaterHeadPoint_E_Fill
        AREA_FE(4,:) = WaterHeadPoint_F_Fill
        
        call polyarea(5,Points_EF(:,1),Points_EF(:,2),F_E(1))
        F_E(1) = F_E(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
    
    !��A-F֮��
    if(F(2)<WL_Up.and.WL_Up<=A(2)) then
        !****A-F֮��ľ�ˮѹ��****!
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
        
        !A�㾲ˮѹ��������
        WaterHead_A = Density*Gravity*(WL_Up-Zero_WaterLevel_Point(2))
        WaterHead_VectorA = Unit_VectorFA*WaterHead_A
        WaterHeadPoint_A = WaterHead_VectorA + Zero_WaterLevel_Point
        
        !F�㾲ˮѹ��������
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorFA*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        Points_FA(1,:) = Zero_WaterLevel_Point
        Points_FA(2,:) = F
        Points_FA(3,:) = WaterHeadPoint_F
        Points_FA(4,:) = WaterHeadPoint_A
        Points_FA(5,:) = Zero_WaterLevel_Point
        
        !���ų�ͼ
        WaterHeadPoint_A_Fill = WaterHead_VectorA/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        
        AREA_AF = Points_FA
        AREA_AF(3,:) = WaterHeadPoint_F_Fill
        AREA_AF(4,:) = WaterHeadPoint_A_Fill
        
        call polyarea(5,Points_FA(:,1),Points_FA(:,2),A_F(1))
        A_F(1) = A_F(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
        
        !****F-E֮��ľ�ˮѹ��****!
        Unit_Line = (F-E)/NORM(F-E,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,-1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorEF = Unit_Vector3D(1:2)
        
        !F�㾲ˮѹ��������
        WaterHead_F = Density*Gravity*(WL_Up-F(2))
        WaterHead_VectorF = Unit_VectorEF*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + F
        
        !E�㾲ˮѹ��������
        WaterHead_E = Density*Gravity*(WL_Up-E(2))
        WaterHead_VectorE = Unit_VectorEF*WaterHead_E
        WaterHeadPoint_E = WaterHead_VectorE + E
        
        Points_EF(1,:) = F
        Points_EF(2,:) = E
        Points_EF(3,:) = WaterHeadPoint_E
        Points_EF(4,:) = WaterHeadPoint_F
        Points_EF(5,:) = F
        
        !���ų�ͼ
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + F
        WaterHeadPoint_E_Fill = WaterHead_VectorE/Load_Fill_Factor + E
        
        AREA_FE = Points_EF
        AREA_FE(3,:) = WaterHeadPoint_E_Fill
        AREA_FE(4,:) = WaterHeadPoint_F_Fill
        
        call polyarea(5,Points_EF(:,1),Points_EF(:,2),F_E(1))
        F_E(1) = F_E(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
    
    !��F-E֮��
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
        
        !F�㾲ˮѹ��������
        WaterHead_F = Density*Gravity*(WL_Up-Zero_WaterLevel_Point(2))
        WaterHead_VectorF = Unit_VectorEF*WaterHead_F
        WaterHeadPoint_F = WaterHead_VectorF + Zero_WaterLevel_Point
        
        !E�㾲ˮѹ��������
        WaterHead_E = Density*Gravity*(WL_Up-E(2))
        WaterHead_VectorE = Unit_VectorEF*WaterHead_E
        WaterHeadPoint_E = WaterHead_VectorE + E
        
        Points_EF(1,:) = Zero_WaterLevel_Point
        Points_EF(2,:) = E
        Points_EF(3,:) = WaterHeadPoint_E
        Points_EF(4,:) = WaterHeadPoint_F
        Points_EF(5,:) = Zero_WaterLevel_Point
        
        !���ų�ͼ
        WaterHeadPoint_F_Fill = WaterHead_VectorF/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_E_Fill = WaterHead_VectorE/Load_Fill_Factor + E
        
        AREA_FE = Points_EF
        AREA_FE(3,:) = WaterHeadPoint_E_Fill
        AREA_FE(4,:) = WaterHeadPoint_F_Fill
        
        call polyarea(5,Points_EF(:,1),Points_EF(:,2),F_E(1))
        F_E(1) = F_E(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
    
    !����ˮλ
    !��B��֮��
    if(WL_Down>B(2)) then
        !****B-C֮��ľ�ˮѹ��****!
        Unit_Line = (B-C)/NORM(B-C,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorBC = Unit_Vector3D(1:2)
        
        !B�㾲ˮѹ��������
        WaterHead_B = Density*Gravity*(WL_Down-B(2))
        WaterHead_VectorB = Unit_VectorBC*WaterHead_B
        WaterHeadPoint_B = WaterHead_VectorB + B
        
        !C�㾲ˮѹ��������
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorBC*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        Points_BC(1,:) = B
        Points_BC(2,:) = C
        Points_BC(3,:) = WaterHeadPoint_C
        Points_BC(4,:) = WaterHeadPoint_B
        Points_BC(5,:) = B
        
        !���ų�ͼ
        WaterHeadPoint_B_Fill = WaterHead_VectorB/Load_Fill_Factor + B
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        
        AREA_BC = Points_BC
        AREA_BC(3,:) = WaterHeadPoint_C_Fill
        AREA_BC(4,:) = WaterHeadPoint_B_Fill
        
        call polyarea(5,Points_BC(:,1),Points_BC(:,2),B_C(1))
        B_C(1) = B_C(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
        
        !****C-D֮��ľ�ˮѹ��****!
        Unit_Line = (C-D)/NORM(C-D,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorCD = Unit_Vector3D(1:2)
        
        !C�㾲ˮѹ��������
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorCD*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        !D�㾲ˮѹ��������
        WaterHead_D = Density*Gravity*(WL_Down-D(2))
        WaterHead_VectorD = Unit_VectorCD*WaterHead_D
        WaterHeadPoint_D = WaterHead_VectorD + D
        
        Points_CD(1,:) = C
        Points_CD(2,:) = D
        Points_CD(3,:) = WaterHeadPoint_D
        Points_CD(4,:) = WaterHeadPoint_C
        Points_CD(5,:) = C
        
        !���ų�ͼ
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        WaterHeadPoint_D_Fill = WaterHead_VectorD/Load_Fill_Factor + D
        
        AREA_CD = Points_CD
        AREA_CD(3,:) = WaterHeadPoint_D_Fill
        AREA_CD(4,:) = WaterHeadPoint_C_Fill
        
        call polyarea(5,Points_CD(:,1),Points_CD(:,2),C_D(1))
        C_D(1) = C_D(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
    
    !��B-C֮��
    if(C(2)<WL_Down.and.WL_Down<=B(2)) then
        !****B-C֮��ľ�ˮѹ��****!
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
        
        !A�㾲ˮѹ��������
        WaterHead_B = Density*Gravity*(WL_Down-Zero_WaterLevel_Point(2))
        WaterHead_VectorB = Unit_VectorBC*WaterHead_B
        WaterHeadPoint_B = WaterHead_VectorB + Zero_WaterLevel_Point
        
        !F�㾲ˮѹ��������
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorBC*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        Points_BC(1,:) = Zero_WaterLevel_Point
        Points_BC(2,:) = C
        Points_BC(3,:) = WaterHeadPoint_C
        Points_BC(4,:) = WaterHeadPoint_B
        Points_BC(5,:) = Zero_WaterLevel_Point
        
        !���ų�ͼ
        WaterHeadPoint_B_Fill = WaterHead_VectorB/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        
        AREA_BC = Points_BC
        AREA_BC(3,:) = WaterHeadPoint_C_Fill
        AREA_BC(4,:) = WaterHeadPoint_B_Fill
        
        call polyarea(5,Points_BC(:,1),Points_BC(:,2),B_C(1))
        B_C(1) = B_C(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
        
        !****C-D֮��ľ�ˮѹ��****!
        Unit_Line = (C-D)/NORM(C-D,2)
        Cr1(1:2) = Unit_Line
        Cr1(3) = 0
        Cr2 = [0,0,1]
        call CROSS(Cr1,Cr2,Unit_Vector3D)
        Unit_VectorCD = Unit_Vector3D(1:2)
        
        !C�㾲ˮѹ��������
        WaterHead_C = Density*Gravity*(WL_Down-C(2))
        WaterHead_VectorC = Unit_VectorCD*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + C
        
        !D�㾲ˮѹ��������
        WaterHead_D = Density*Gravity*(WL_Down-D(2))
        WaterHead_VectorD = Unit_VectorCD*WaterHead_D
        WaterHeadPoint_D = WaterHead_VectorD + D
        
        Points_CD(1,:) = C
        Points_CD(2,:) = D
        Points_CD(3,:) = WaterHeadPoint_D
        Points_CD(4,:) = WaterHeadPoint_C
        Points_CD(5,:) = C
        
        !���ų�ͼ
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + C
        WaterHeadPoint_D_Fill = WaterHead_VectorD/Load_Fill_Factor + D
        
        AREA_CD = Points_CD
        AREA_CD(3,:) = WaterHeadPoint_D_Fill
        AREA_CD(4,:) = WaterHeadPoint_C_Fill
        
        call polyarea(5,Points_CD(:,1),Points_CD(:,2),C_D(1))
        C_D(1) = C_D(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
    
    !��C-D֮��
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
        
        !C�㾲ˮѹ��������
        WaterHead_C = Density*Gravity*(WL_Down-Zero_WaterLevel_Point(2))
        WaterHead_VectorC = Unit_VectorCD*WaterHead_C
        WaterHeadPoint_C = WaterHead_VectorC + Zero_WaterLevel_Point
        
        !E�㾲ˮѹ��������
        WaterHead_D = Density*Gravity*(WL_Down-D(2))
        WaterHead_VectorD = Unit_VectorCD*WaterHead_D
        WaterHeadPoint_D = WaterHead_VectorD + D
        
        Points_CD(1,:) = Zero_WaterLevel_Point
        Points_CD(2,:) = D
        Points_CD(3,:) = WaterHeadPoint_D
        Points_CD(4,:) = WaterHeadPoint_C
        Points_CD(5,:) = Zero_WaterLevel_Point
        
        !���ų�ͼ
        WaterHeadPoint_C_Fill = WaterHead_VectorC/Load_Fill_Factor + Zero_WaterLevel_Point
        WaterHeadPoint_D_Fill = WaterHead_VectorD/Load_Fill_Factor + D
        
        AREA_CD = Points_CD
        AREA_CD(3,:) = WaterHeadPoint_D_Fill
        AREA_CD(4,:) = WaterHeadPoint_C_Fill
        
        call polyarea(5,Points_CD(:,1),Points_CD(:,2),C_D(1))
        C_D(1) = C_D(1)/1000.0      !������С��ΪKN
        
        !������ͶӰ�����ñ߽���
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
        
        
