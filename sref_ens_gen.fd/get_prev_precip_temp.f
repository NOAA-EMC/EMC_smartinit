	subroutine get_prev_precip(i00,np,ns,fhead,jf,
     +          precip,mrk_frz,mrk_ice,mrk_snow)
         real precip(jf,ns),mrk_frz(jf,ns),mrk_ice(jf,ns),
     +         mrk_snow(jf,ns),var(jf)
         character*19 fhead,fname
         dimension kpds(25),kgds(25)
         logical lb(jf)
         integer hh(30)

         data (hh(i),i=1,30)
     +   /0, 3, 6, 9, 12, 15, 18, 21, 24,
     +    27, 30, 33, 36, 39, 42, 45, 48, 51,
     +    54, 57, 60, 63, 66, 69, 72, 75, 78,
     +    81, 84, 87/

         iunit=60
                 
         fname=trim(fhead)    
         write(*,*)'In prev precip: ', fname   
         call baopenr(iunit,fname,ierr)
         iseek=0
         call skgb(iunit,iseek,llgrib,llskip)
         DO while(llgrib.gt.0)  ! llgrib
           call rdgb(iunit,llgrib,llskip,kpds,kgds,jff,lb,var)
           call skgb(iunit,iseek,llgrib,llskip)
 
           do n=1,np
            i0=i00+1-n
            ihh=hh(i0)
            if(kpds(5).eq.61.and.kpds(15).eq.ihh) then
               precip(:,i0) = var (:)
            end if
            if(kpds(5).eq.141.and.kpds(14).eq.ihh) then
               mrk_frz(:,i0) = var(:)
            end if
            if(kpds(5).eq.142.and.kpds(14).eq.ihh) then
               mrk_ice(:,i0) = var(:)
            end if
            if(kpds(5).eq.143.and.kpds(14).eq.ihh) then
               mrk_snow(:,i0) = var(:)
            end if
           end do

          END DO
 
          call baclose(iunit,ierr)

        return
        end

        subroutine get_prev_temp(hr,fhead,jf,p,
     +     t2m4cooling,t4cooling)
         real t4cooling(jf,14),t2m4cooling(jf),var(jf)
         character*19 fhead,fname
         integer p(14)
         character*2 hr
         dimension kpds(25),kgds(25)
         logical lb(jf)

         iunit=60
         fname=trim(fhead)//'.f'//hr
         write(*,*)'In prev temp', fname
         call baopenr(iunit,fname,ierr)
         iseek=0
         call skgb(iunit,iseek,llgrib,llskip)
         DO while(llgrib.gt.0)  ! llgrib
           call rdgb(iunit,llgrib,llskip,kpds,kgds,jff,lb,var)
           call skgb(iunit,iseek,llgrib,llskip)

            do k=1,14
             if(kpds(5).eq.11.and.kpds(6).eq.100.and.
     +         kpds(7).eq.p(k) ) then
                 t4cooling(:,k) = var(:)
             end if
            end do

            if(kpds(5).eq.11.and.kpds(6).eq.105.and.
     +         kpds(7).eq.2 ) then
                 t2m4cooling = var
             end if
          END DO

          call baclose(iunit,ierr)

         return
         end
