
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
cc  SUBROUTINE get_new_fog: implemented based on Zhou and Ferrier 's aysmptotic formulation
cc      2008 on JAMC                     
cc        
cc     Input: grid-wide u,v component profile and surface values, temperature and RH profiles
cc            and surface values, surface height, and previous time step temperature profile 
cc            and surface values (are used to compute cooling rate).
cc     Output: fog LWC
cc
cc
cc    This code is for grid or point-wide computation as following steps:
cc 
cc         (1) Search for which (pressure)levels the surface height is located      
cc         (2) Search for which (pressure)levels the suturated top above the surface is located 
cc                and get the fog layer (saturated layer) depth, if its depth > 800m, not 
cc                a fog but reture, otherwise              
cc         (3) Search from the fog layer top downward to see if its bottom reach the ground
cc             if its bottom not touch the ground and > 800m, it is not a fog but return, 
cc             otherwise 
cc         (4) Compute averaged cooling rate within the fog layer using current and previous 
cc             time step's temperature profiles and surface (in C/hr) 
cc         (5) Based on temperaure at nearest level from the fog top and 2m to compute
cc             averag temperature with the fog layer, based on which parameter beta is computed
cc       (5.1) From cooling rate, fog depth and beta, the critical turb exch. ceoff Kc can 
cc               be computed
cc         (6) Based on the u,v at nearest level from the fog top and 10m u,v, to compute   
cc             wind speeds at the fog top and the surface
cc         (7) Based on T, wind speeds at nearest level from the fog top and 2m, Ri is computed          
cc             from which stability function Fm(Ri) is computed, from which turbulent excahnge
cc             coefficient K is computed
cc       (7.1) If won't further compute LWC, using K and Kc, fog exists or not can be determined
cc              at this step (K>Kc, K<Kc?), otherwise,     
cc         (8) With K, cooling rate, beta, fog boundary layer FBL can be computed
cc         (9) From the FBL, cooling rate, fog depth, LWC profile with the fog layer are computed 
cc        
cc    Language: Fortran 77/90
cc
cc    Origninal author: Binbin Zhou, EMC/NCEP/NOAA
cc                      August 28, 2010
cc
cc    Modifcation history: 
cc        Sept 9, 2010 by B. Zhou:
cc          Add startus cloud layer checking (< 400m), if yes, use modeled cloud layer as fog layer
cc               instead of using RH to check saturated layer as fog layer. This imply the modeled cloud
cc               depth is assumed correct but just adjust its LWC distrbution   
cc               If clout top < 400m and cloud bottm < 50 m above the ground,
cc               find whcih level the cloud top is located, then repeat the above steps from
cc               step (4)
cc
cc        Oct. 10, 2010 by B. Zhou:
cc          Add LWC advection term from RH or specific humidity field into the Kc and LWC
cc               computations
cc
cc
cc
cc   Description diagram:  
cc
cc             kp and kp+1: the level just below and abobe the surcae
cc                    kfog: fog top level 
cc   (1) Clear sky case
cc
cc  ---------------------------------------------------------------------------- k=14, hp(14)
cc
cc   .....................................................                       .....
cc                                        
cc  ------------ fog top---100%-------------- kfog (=4 in this case)------------ k=4, hp(4)
cc         ^                             ^   
cc         |                             |
cc   ------|-------------- 100%----------|--- kp+1 (=3 in this case)------------ k=3, hp(3)
cc      depth_fog=z_RH-hsfc              |   
cc         | ............ o-o (10m)      |
cc         |   ^  ....[2m] |            z_RH=hp(kfog)
cc      ___v___|____^___Y__|_____________|___________________________ surface level (hsfc)
cc     /       |    |                    |                           \
cc-------------|----|--------------------|---kp (=2 in this case) --------------- k=2, hp(2)                                                \
cc   /         |   z2m=2+hsfc            |                             \
cc  /          |    |                    |                              \          
cc /        z10m=10+hsfc                 |                               \
cc ------------|----|--------------------|-----------------------------------------k=1, hp(1)
cc/            |    |                    |                                 \
cc ____________v____v____________________v__________________________________\______sea level
cc|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
cc
cc  (2) Stratus cloud case (cloud top: cldt<400m)
cc
cc  ~~~~~~~cloud top (cldt < 400m)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cc         ^                             ^
cc         |                             |
cc   -kfog-|- (level just below cldt) ---|---------------------------------------k=4. hp(4)
cc         |                             |
cc         |                             |
cc      depth_fog=cldt (i.e. z_RH-hsfc)  |
cc         |                             |
cc  -------|-----------------------------|----------------------------------------k=3, hp(3)
cc         |                             |
cc         | ............ o-o (10m)      |
cc         |   ^  ....[2m] |            z_RH=cldt+hsfc
cc      ___v___|____^___Y__|_____________|___________________________ surface level (hsfc)
cc     /       |    |                    |                           \
cc-------------|----|--------------------|---kp (=2 in this case) --------------- k=2, hp(2)                                                \
cc   /         |   z2m=2+hsfc            |                             \
cc  /          |    |                    |                              \
cc /        z10m=10+hsfc                 |                               \
cc ------------|----|--------------------|-----------------------------------------k=1, hp(1)
cc/            |    |                    |                                 \
cc ____________v____v____________________v__________________________________\______sea level
cc||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


        subroutine get_new_fog(up,vp,hp,u10,v10,hsfc,
     +              rhp,rh2,t2,tp,interval,lv,cldt,cldb,adv,
     +             lwc)

         parameter (Rd = 287.0, Rv=416.0, Ro=1.2)

c                    Input
c         up,vp : u,v on pressure levels
c            hp : height of pressure levels
c       u10,v10 : 10m u and v
c          hsfc : sfc height
c            tp : previous and current temperature on  pressure levels
c                 tp(1:) presious, tp(2:) current temerature
c           rhp : RH on pressure levels
c            t2 : previous and current temperature at 2m
c                 t2(1) previous, t2(2) current temerature
c           rh2 : previous and current RH at 2m
c          cldt : cloud top height above the ground
c          cldb : cloud base height above the ground
c       interval: time step interval (in hours)    
c            lv : number of vertical levels
c           adv : advection term
c                    Output
c           lwc : fog LWC
c


        INTEGER lv,interval
        REAL up(lv),vp(lv),hp(lv),rhp(lv),tp(lv,2)
        REAL u10,v10,hsfc,lwc,rh2,t2(2),cooling_rate,
     +       Km,lamda_1,lamda,lwc_max,Kc,cldt,cldb

        real rhthr

        rhthr=95.0         !set 95% as satuated

c        adv=0.0

          if(t2(2).lt.-20.0) then                  !ice fog saturation threshold
            ttw=17.62*t2(2)/(243.12 + t2(2))     !over water 
            tti=22.46*t2(2)/(272.62 + t2(2))     !over ice
                                                 !esi = 6.112 *2.71828 ** tti
                                                 !esw = 6.112 *2.71828 ** ttw
            rhthr =2.71828**(tti-ttw)*100.         ! rhthr = esi / esw
          end if


c        do k=1,14
c         write(*,30) hp(k),tp(k,2),rhp(k),up(k),
c     +      vp(k),tp(k,1)
c        end do
c        write(*,30) hsfc,t2(2),rh2,u10,v10,t2(1),cldt,cldb

30     format(5x,f6.1, 5f5.1)

        lwc = 0.0
c   search sfc in which pressure layers     
c         hp(kp)   : the  level just below the surface level
c         hp(kp+1) : the  level just above the surface level

         z10m = 10. + hsfc
         z2m  =  2. + hsfc

c         write(*,*)'z10m=',z10m,'z2m=',z2m
          if (z10m .lt. hp(1) ) then
            kp = 0
          else
            do k = 1,lv-1
             if(z10m.ge.hp(k).and.z10m.lt.hp(k+1)) then
               kp = k
             end if
            end do
          end if

c          write(*,*)'kp=',kp  
      
cc
cc  Now see if there is low stratus cloud near the surface
cc         
          if(cldt.lt.400.0.and.cldb.lt.50.0) then    !yes there is stratus cloud case
            kfog=0
            z_RH=cldt+hsfc
            !seach which level the cloud top is below
            do k = 1, lv-1
             if(z_RH.ge.hp(k).and.z_RH.lt.hp(k+1)) then
               kfog = k+1
             end if
            end do
           goto 1000                           
          end if

c   search RH=100% (95%) depth above the sfc and averaged cooling rate
c         hp(kfog) : the  level just above the fog top

          z_RH=hsfc  
          kfog=0
          do k = 1, lv-1
            if(rhp(k).ge.rhthr.and.
     +         rhp(k+1).lt.rhthr ) then
              kfog=k           !find which pressure level is saturated
              z_RH = hp(kfog)  
            end if
          end do

c           write(*,*) 'z_RH-hsfc=',z_RH-hsfc
          if((z_RH-hsfc).gt.800.0) return            !not a fog layer but a stratus cloud

          !now search from fog top downward to find fog bottom reach the ground?
          kbottom=0
          if(kfog.ge.2) then
            do k = kfog,1,-1
             if (rhp(k).lt.rhthr) then
              kbottom=k+1
              goto 100
             end if
            end do
           else
             kbottom=1
             goto 100
          end if

100       continue
          if(rh2.gt.rhthr) then
            touch_down=0.
          else
            touch_down=hp(kbottom)-hsfc
          end if

c          write(*,*) 'touch_down=',touch_down
          if(touch_down.gt.100.0) return               !not a fog layer but a stratus cloud 
     
1000      continue
 
c          write(*,*)'z_RH=',z_RH,'kfog=',kfog,
c     +    'rh2=',rh2,'z2m=',z2m

          if(z_RH.le.z2m) then
            if(rh2.ge.rhthr) then 
              depth_fog = 2.0
              cooling_rate=(t2(1)-t2(2))/interval   !in C/hr 
              tfog=t2(2)
c              if (cooling_rate.le.0.0) return 
            else
              depth_fog = 0.0
              return
            end if 
          else
             depth_fog = z_RH - hsfc
             cooling_rate=( (tp(kfog,1)-tp(kfog,2)) +
     +         (t2(1)-t2(2)) )/interval/2.0
             tfog=(tp(kfog,2)+t2(2))/2.0
c             if (cooling_rate.le.0.0) return
          end if



c          write(*,*)'depth_fog=',depth_fog,
c     +      'cooling_rate=',cooling_rate,
c     +      'tfog=',tfog

C          tt=7.45*tfog/(235.0 + tfog) 
C        Use WMO recommended formulation (2008)
C   Guide to Metteorological Instruments and Methods of
C   Observation (CIMO Guide):

          if( tfog.ge.0.0) then
            tt=17.62*tfog/(243.12 + tfog)     !over liquid water
          else
            tt=22.46*tfog/(272.62 + tfog)     !over ice, assume no supercooled water
          end if

          es = 6.112 *2.71828 ** tt
          Tk=tfog + 273.17
          beta = 622.0* 2.5e6 * es / (Rv*Tk*Tk*1000)

          c_adv=beta*cooling_rate/3600.0 + adv/1000.0

          if(c_adv.lt.0.0) then
            lwc=0.0
            return
          end if

          lwc_max = sqrt(c_adv*depth_fog/0.062)

c          write(*,*)'tt=',tt,'es=',es,'Tk=',Tk,
c     +      'beta=',beta,'lwc_max=',lwc_max

c   compute FBL (Fog Boundary Layer, delta)
          if ( depth_fog .le. 2.0 ) then
            wp=sqrt(up(kp+1)*up(kp+1)+vp(kp+1)*vp(kp+1))
            w10=sqrt(u10*u10+v10*v10)
            dwdz=(wp-w10)/(hp(kp+1)-z10m)
            dtdz=(tp(kp+1,2)-t2(2))/(hp(kp+1)-z2m)
          else
            wp=sqrt(up(kfog)*up(kfog)+vp(kfog)*vp(kfog))
            w10=sqrt(u10*u10+v10*v10)
            dwdz=(wp-w10)/(hp(kfog)-z10m)
            dtdz=(tp(kfog,2)-t2(2))/(hp(kfog)-z2m)
          end if
          
          dwdz=abs(dwdz)
          if(dwdz.eq.0.0) dwdz=0.001

c          write(*,*)'wp=',wp,'w10=',w10,'dwdz=',dwdz,
c     +       'dtdz=',dtdz  

          Ri = (9.8/Tk)*(dtdz+0.01)/dwdz/dwdz 
          if (Ri.le.0.0) then                        !Unstable case: Beljaars: "The parameterization 
           z=hp(kp+1)-hsfc                           !of the planetary boundary layer", May 1992, ECMWF report
           y=(1.0+z/0.02)
           Ch=12.0*sqrt(y)/(log(y)*log(y))
           Fm=1.0-10.0*Ri/(1.+Ch*sqrt(-Ri))
          else                                       !Stable case: Holtslag, et al. BLM 2006, vol. 118
c           Fm=1.0/(1.0+10.0*Ri)                     !long tail format, Ric may > 0.25 this leads to higher turbulence
                                                     !and fog LWC becomes smaller or dispersed
 
            if(Ri.ge.0.0.and.Ri.lt.0.1) then         !Sharp tail (Ric ~ 0.25)
             Fm=(1.0-5.0*Ri)*(1.0-5.0*Ri)
            else
             Fm=(1.0/20.0/Ri)*(1.0/20.0/Ri)
            end if
          end if

c           write(*,*)'Ri=',Ri,'FM=',FM

         
         lamda_1=1./(0.4*(depth_fog+0.02)) + 1./40.0
         lamda=1.0/lamda_1
          Km=lamda*lamda*dwdz*Fm
          FBL=Km/2.0/
     +     sqrt(0.062*c_adv*depth_fog)

          Kc=1.38*sqrt(0.062*c_adv)*
     +        depth_fog**1.5

c          write(*,*)'Km=',Km,'Kc=',Kc,
c     +     'lamda=',lamda,'FBL=',FBL

c  at last compute LWC near the surface (at z = 1.0 m)
         if(depth_fog.le.2.0) then
            z=1.0
         else
            z=0.2*depth_fog
         end if
         
         ax=sqrt(1.0-z/depth_fog)
         cx=2.0/(1.0+exp(z/FBL))

         lwc=lwc_max*(ax - cx)                    

c         write(*,*) 'lwc=',lwc

c         dz=depth_fog/10.0
c         do k=0,10
c          z=dz*k
c          prof_lwc=lwc_max*(sqrt(1.0-z/depth_fog) -     
c     +       2.0/(1.0+exp(z/FBL)))
c          if (prof_lwc.lt.0.0) prof_lwc=0.0
c          write(*,*) z, 'LWC=',prof_lwc
c         end do

         if (lwc.lt.1.0E-2) lwc = 0.0

       return
       end

