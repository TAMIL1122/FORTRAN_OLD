!1234 PROGRAM FOR NEWTON BACKWARD INTERPOLATION
      DIMENSION X(20),Y(20),BD(20,20)
      WRITE(*,*)'TYPE THE VALUE OF N'
      READ(*,2)N
    2 FORMAT(I2)
      WRITE(*,*)'TYPE Xi,Yi'
      DO 10 I=1,N
      READ(*,4) X(I), Y(I)
    4 FORMAT(2F15.5)
   10 CONTINUE
      WRITE(*,*)'THE BACKWARD DIFFERENCE TABLE IS'
      DO 15 I=1,N
      BD(1,I)=Y(I)
   15 CONTINUE
      DO 20 I=2,N
      DO 25 J=1,N-I+1   
      BD(I,J)=BD((I-1),(J+1))-BD((I-1),J)
   25 CONTINUE
   20 CONTINUE
      DO 30 J=1,N
      WRITE(*,6)(BD(I,J),I=1,N-J+1)
    6 FORMAT(1X,5F15.2)
   30 CONTINUE
      WRITE(*,*)'ENTER THE VALUE OF X TO INTERPOLATE' 
      READ(*,8)XVALUE
    8 FORMAT(F15.2)
      H=X(2)-X(1)
      V=(XVALUE-X(N))/H
      VVALUE=V
      FACT=1.0
      OUTPUT=BD(1,N)+(V*BD(2,(N-1)))
      DO 60 I=3,N
      VVALUE=VVALUE*(V+(I-2)) 
      FACT=FACT*(I-1)
      OUTPUT=OUTPUT+(VVALUE*BD(I,(N-I+1)))/FACT
   60 CONTINUE
      WRITE(*,9)XVALUE,OUTPUT
    9 FORMAT(1X,'THE INTERPOLATED VALUE AT X IS:',F15.5)
      STOP 
      END
                      