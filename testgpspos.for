        program testgpspos
        parameter(nerr=50)
        integer*4 ierror(nerr), ireturn, ichoosedrop
!
        ichoosedrop = 20
        print *, 'ichoosedrop =', ichoosedrop
        CALL gpspos(ierror,ireturn,ichoosedrop)
        print *, 'ierror =', ierror
        print *, 'ireturn =', ireturn
        stop
        end
! ----------------------
