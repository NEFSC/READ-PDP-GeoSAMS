module MortalityMod              
    use globals
    implicit none
    type Mortality_Struct
       real(dp) NaturalMortality(max_size_class)
       real(dp) Incidental
       real(dp) Discard(max_size_class)
       real(dp) F(max_num_years)
       real(dp) a(max_num_years),b(max_num_years),c(max_num_years),d(max_num_years)
       integer Stype(max_num_years)
       integer Year(max_num_years)
       real(dp) select(max_size_class)
       character(2) region
       logical is_closed
       integer num_size_classes, num_years
       real(dp) natural_mort_adult,natural_mort_juv
       real(dp) alpha(1:max_size_class)
       integer management_Area
    end type Mortality_Struct
   end module
   