real*8 function atof(String)
!************************************************************************
!*  Convert String to Double Precision Value                            *
!************************************************************************
    implicit none
    character(*),intent(in)::String
    integer::       countTot,   countInt,   countDec,   count,  kount,  &
                    ck,         dot,        digit,      atoi,   Option
    real(8)::       atofi
    character(2)::  outbuf
    character(8)::  form = '(f98.90)'
!========================================================================
    Option = 1
            
    select case(Option)

        case(1)   !Type 1
    
            read(String,*) atof
                
            !print *,"atof1= ",atof

        case(2)   !Type 2

            !count dot digit
            dot = max (1, index(String, '.'))
            countTot = len(trim(String))
            countInt = max(1, dot - 1)
            countDec = max(1, countTot - dot)
            
            write(outbuf, '(i2)' ) countTot;    form(3:4) = outbuf
            write(outbuf, '(i2)' ) countDec;    form(6:7) = outbuf
            read(String,form) atof
            
        !print *,"atof2= ",atof

        case(3)   !Type 3

            !count dot digit
            dot = max (1, index(String, '.'))
            countTot = len(trim(String))
            countInt = max(1, dot - 1)
            countDec = max(1, countTot - dot)
            
            !Conver interger character to value
            atoi  = 0           
            do count=1,countInt
                if (String(count:count) >= '0' .and. String(count:count) <= '9') then
                    read(String(count:count),*) digit
                    atoi = 10*atoi + digit
                endif
            enddo

            !Conver decimal character to value
            atof  = 0.0
            kount = 1
            do count=dot+1,countTot
                atofi = 1.0
                do ck=0,kount-1
                    atofi = 0.1*atofi
                enddo
                read(String(count:count),*) digit
                atof = atof + digit*atofi
                kount = kount + 1
            enddo
            
        atof = atof  + dble(atoi)
            
        !print *,"atof3= ", atof  
            
    end select            
!========================================================================          
end function