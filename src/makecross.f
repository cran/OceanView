       INTEGER FUNCTION Checkit(val, old, ipos)
       DOUBLE PRECISION val, old(*), MV, NewV
       INTEGER I

       Checkit = 0
       if (ipos .eq. 0) return 

       DO I = 1, ipos
         if (val .EQ. old(I)) THEN
           Checkit = I
           return
         endif
       ENDDO
       Checkit = 1
       MV = abs(val - old(1))
         DO I = 2, ipos
           NewV = abs(val - old(I))
           IF (NewV .LT. MV) THEN
             MV = NewV
             Checkit = I
           ENDIF
         ENDDO  
       
       END FUNCTION


       SUBROUTINE crosstab(input, ninput, colnr, rownr, valnr,                   &
     &                    cols, rows, nr, nc, cross, count, NAnum)

       IMPLICIT NONE
       INTEGER colnr, rownr, valnr, nr, nc, ninput
       DOUBLE PRECISION input(3,*), rows(nr), cols(nc), NAnum,                   &
     &    cross(nr, nc)

       INTEGER count(nr, nc)
       INTEGER I, J , icol, irow
       DOUBLE PRECISION lrow, lcol
       INTEGER Checkit
  
         DO I = 1, ninput
           if (abs(input(valnr,i) - NAnum) > 1) THEN
             lrow = input(rownr,i)
             irow = Checkit(lrow, rows, nr)
       !     IF (irow .eq. 0) THEN
       !       call rwarn(" do not find row value")
       !	   ENDIF

             lcol = input(colnr,i)
             icol = Checkit(lcol, cols, nc)
       !     IF (icol .eq. 0) THEN
       !       call rwarn(" do not find column value")
       !	   ENDIF

             cross(irow, icol) = cross(irow, icol) + input(valnr,i)
             count(irow, icol) = count(irow, icol) + 1
           ENDIF
         ENDDO
         DO I = 1, nr
           DO J = 1, nc
              IF (count(I, J) .EQ. 0) THEN
                cross(I, J) = NAnum
              ELSE
                cross(I, J) = cross(I, J)/count(I, J) 
              ENDIF                                
           ENDDO
         ENDDO  

       END SUBROUTINE crosstab


       SUBROUTINE crosstab2(input, ninput, colnr, rownr, valnr,                  &
     &           cols, rows, nr, nc, nrow, ncol, indrow, indcol,                 &
     &           cross, count, NAnum)

       IMPLICIT NONE
       INTEGER colnr, rownr, valnr, nr, nc, nrow, ncol, ninput
       DOUBLE PRECISION input(3,*), rows(nrow), cols(ncol), NAnum,               &
     &    cross(nr, nc)

! indrow = index to row of rowvalues (several rowvalues can point to same row)
! nr <= nrow; nr = number of rows in crosstable;
! nrow = number of unique rowvalues in input

       INTEGER count(nr, nc), indrow(nrow), indcol(ncol)
       INTEGER I, J , icol, irow
       DOUBLE PRECISION lrow, lcol
       INTEGER Checkit
  
         DO I = 1, ninput
           if (abs(input(valnr,i) - NAnum) > 1) THEN
             lrow = input(rownr,i)
             irow = Checkit(lrow, rows, nrow)
       !     IF (irow .eq. 0) THEN
       !       call rwarn(" do not find row value")
       !	   ENDIF

             lcol = input(colnr,i)
             icol = Checkit(lcol, cols, ncol)
       !     IF (icol .eq. 0) THEN
       !       call rwarn(" do not find column value")
       !	   ENDIF
             irow = indrow(irow)
             icol = indcol(icol)
             cross(irow, icol) = cross(irow, icol) + input(valnr,i)
             count(irow, icol) = count(irow, icol) + 1
           ENDIF
         ENDDO
         DO I = 1, nr
           DO J = 1, nc
              IF (count(I, J) .EQ. 0) THEN
                cross(I, J) = NAnum
              ELSE
                cross(I, J) = cross(I, J)/count(I, J) 
              ENDIF                                
           ENDDO
         ENDDO  

       END SUBROUTINE crosstab2
