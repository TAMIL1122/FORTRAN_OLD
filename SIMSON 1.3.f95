!1234 PROGRAM FOR SIMPSON'S 1/3 RULE
  DIMENSION X(100),Y(100)
  WRITE(*,*)'ENTER THE VALUE OF N,X0,Y0'
  READ(*,*)N,X0,Y0
  G1=0.0
  G2=0.0
  G3=0.0
  WRITE(*,*)'TYPE THE VALUE OF X AND Y'
  DO 10 I=1,N-1
  WRITE(*,*)"X",I,"="
  READ(*,*)X(I)
  WRITE(*,*)"Y",I,"="
  READ(*,*)Y(I)
  IF(I.EQ.N-1)THEN
  G1=Y0+Y(I)
  WRITE(*,*)"G1=",(G1)
  ELSE
  IF(MOD(I,2).EQ.0)THEN
  G3=G3+Y(I)
  WRITE(*,*)"G3=",(G3)
  ELSE
  G2=G2+Y(I)
  WRITE(*,*)"G2=",(G2)
  END IF
  END IF
10 CONTINUE
  H=(X(1)-X0)
  G=((H/3.0)*(G1+(4*G2)+(2*G3)))   
  WRITE(*,5)G
 5 FORMAT(2X,"THE INTEGRAL VALUE IS",F15.5)
  STOP 
  END

    
  