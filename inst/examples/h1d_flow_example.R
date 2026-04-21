#this example comes with hydrusR
#TDB, 2026-04-14
# - the data it generates (see bottom) is very strange, making me wonder if there is an error in the hydrusR code, an error in Hydrus, or that
#     I don't understand what the example is trying to show
# - nonetheless, it does successfully run Hydrus for a single material profile with roots and ET.
# - I haven't made any substantive changes.
# --  added comments and the section at bottom to load and plot outputs.
# --  edited the call to create.soil.profile to give the same outputs as original using my updated function.

library(hydrusR); load_pkg()
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))

## Basic inputs
TimeUnit = "cm" ## Space units
SpaceUnit = "hours" ## time units
PrintTimes = 1

profile_depth = 200
endTime = 500
deltaz = 1
rdepth = 100
time_step = 0.25
soil_para = list(thr = 0.045, ths = 0.43,
                 Alfa = 0.145, n = 2.69, Ks = 29.7, l = 0.45)


# hydrus_path =  "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"

project_name = "h1dExample5"
parent_dir = path.expand("~/temp")
project_path = path.expand(file.path(parent_dir, project_name))


##Process inputs
rwu = T   ###  rootwater uptake

##Profile/geometry inputs
profile_nodes = seq(0, profile_depth, by = deltaz)
initial_wtable = 30
obs_nodes_all = seq(20, profile_depth, by = 20)
nObsNodes = length(obs_nodes_all)
rooting_depth = 120

### Time inputs
total_timesteps = endTime/time_step
ntimesteps = length(1:total_timesteps)

## LAI and pet
input_pet = TRUE #this doens't work sensibly. if this is true then it assigns LAI to rRoot in atm_bc_data. rRoot is the potential transpiration rate
LAI = 4.0        # in cm/hr. 4 cm/hr is an impossible PET. to work properly, it would need to use LAI as some sort of modifying function to the baseline PET rate. e.g. rRoot = PET_max * f(LAI)
et_rate = 0.6

## Boundary conditions inputs
const_botbc = TRUE
bot_bc_type = "flux"
const_botFlux = 0.0000 ##### in cm/hr

### Atmospheric top boundary conditions
### Time variable boundary conditions.
atm_bc_data = data.frame(tAtm = seq(time_step, endTime, time_step), #time, same units as project
                         Prec = numeric(ntimesteps), #precipitation in L/T units
                         rSoil = numeric(ntimesteps), #potential evap rate in L/T units
                         rRoot = numeric(ntimesteps), #potential transpiration in L/T. rRoot+rSoil = PET
                         hCritA = rep(10000, ntimesteps), #Absolute value of the minimum allowed pressure head at the soil surface [L]. unsure what effect this has, if any, so long as it is sufficiently dry. Hydrus manual shows 1e6 not 1e4 like here.
                                                          # per manual - this must be at least theta_r+0.005 for solution stability. also must be drier than P3 value in root stress fn, which is
                                                          # typically between 8000-16000, depending on reference crop settings
                         rB = numeric(ntimesteps), #flux rate at bottom, in L/T
                         hB = numeric(ntimesteps), #pressure head at bottom, in L units
                         ht = numeric(ntimesteps), #pressure head at top, in L units
                         RootDepth = numeric(ntimesteps)) #I don't think this is necessary for this dataset.

#this would be for daily timestep because no hourly modifications made.
if(isTRUE(input_pet)) {
      atm_bc_data$rRoot = rep(LAI, nrow(atm_bc_data))
      atm_bc_data$rSoil = rep(et_rate/2, nrow(atm_bc_data))
} else {
      atm_bc_data$rRoot = rep(et_rate/2, nrow(atm_bc_data))
      atm_bc_data$rSoil = rep(et_rate/2, nrow(atm_bc_data))

}

################## for hourly time units
const_et = rep(et_rate, 365) #same daily value for 1 year
hourly_et =  pet.hourly(pet.daily = const_et) #hourly values for the year, varying throughout the day
# hourly_et$doy <- floor( (0:(nrow(hourly_et)-1))/24) #add doy column - my edit to make plot below
# #show that pet is same diurnal pattern across the year. (added print message to fn to fix later)
# hourly_et %>%
#   filter(doy %in% seq(1,180,30)) %>%
#   ggplot(aes(x = hour, y = pet, color = doy)) +
#   geom_line() +
#   facet_wrap(~doy)
hourly_et = hourly_et[rep(seq_len(nrow(hourly_et)), each = 1/time_step), ] ### for 0.25 time steps

#hourly_et$pet <- hourly_et$pet * time_step #decrease PET needed?  proportional to the number of reps it just got above, or else an hourly rate of 0.1 cm/hr will be replicated for all quarter hours.
# I think this isn't needed because Hydrus is looking for a rate (e.g. cm/hr) not a depth for each time period

hourly_et$rSoil = hourly_et$pet/2 #half of PET is evap
hourly_et$rRoot = hourly_et$pet/2 #half is transpiration

if(isTRUE(input_pet)) { #this doesn't work properly. see notes above where input_pet is declared.
      #atm_bc_data$rRoot = rep(LAI, nrow(atm_bc_data)) #overriding this because it is way too high
      atm_bc_data$rRoot = hourly_et$rRoot[1:nrow(atm_bc_data)]
      atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]
} else {
      atm_bc_data$rRoot = hourly_et$rRoot[1:nrow(atm_bc_data)]
      atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]

}

if(isTRUE(const_botbc)){
      atm_bc_data$hB = rep(0, nrow(atm_bc_data)) #this sets the bottom boundary to always have head = 0
}
atm_bc_data = atm_bc_data[1:ntimesteps, ] #this trims to the correct number of rows.



#### Creates a blank hydrus project with three files
create.H1D.project(project.name = project_name,
                   parent.dir = parent_dir,
                   TimeUnit = TimeUnit,
                   PrintTimes = PrintTimes,
                   overwrite = T,
                   processes = c(WaterFlow = T, RootWaterUptake = rwu),
                   geometry = c(ProfileDepth = profile_depth,
                                NumberOfNodes = length(profile_nodes),
                                ObservationNodes = nObsNodes))

### create the soil profile (PROFILE.DAT) info
# create.soil.profile(project.path = project_path,
#                     profile.depth = profile_depth,
#                     dz = deltaz, obs.nodes = NULL)
# above is the original call. it needs to change because I've edited the create.s.p fn.
# these settings below should make the same output as the original.
create.soil.profile(project.path = project_path,
                    profile.depth = profile_depth,
                    dz = 1,
                    depth.vec = NULL,
                    mat = 1,
                    Head = 0, #same as original
                    obs.nodes = NULL)

#add obs nodes to bottom of PROFILE.DAT
write.obs.nodes(project.path = project_path, obs.nodes = obs_nodes_all) #this causes problems because it leaves a lot of NAs down the row names margin. unsure why. haven't looked.

#edit Head column of PROFILE.DAT to put water table at 30 cm
write.ini.cond(project.path = project_path, wt.depth = initial_wtable)

#edit Beta column of PROFILE.DAT to add roots to 120 cm
write.root.dist(project.path = project_path,  rdepth = rooting_depth, rBeta = 0.962)

write.hydraulic.para(project.path = project_path, para = soil_para)

write.bottom.bc(constant.bc = TRUE, bc.type = bot_bc_type,
                bc.value = const_botFlux, project.path = project_path)

write.atmosph.in(project.path = project_path, maxAL = 2000, deltaT = time_step,
                 atm.bc.data = atm_bc_data[1:2000, ])

##### Default hydrus path in Windows
run.H1D.simulation(project.path = project_path,
                   profile.depth = profile_depth,
                   beginT = 0, endT = endTime, deltaT = time_step,
                   bot.bc.type = bot_bc_type, bot.bc.value = const_botFlux,
                   const.bot.bc = TRUE, atm.bc.data = atm_bc_data,
                   TimeUnit = TimeUnit,
                   show.output = T)



#read outputs and plot them
#################################
library(ggplot2)
df.n <- read.nod_inf(project.path = project_path)
# - this data really looks weird but maybe the example has problems???
# -- post note: I found at least one problem wherein rRoot was being assigned 4 cm/hr, which was causing huge moisture fluctuations
#               in the upper layer. I fixed this and now the fluctuations are smaller (0-200 cm not 0-10000 cm). still they remain and
#               my notes below about possible capillary recharge as the influx source still seem valid.

#Moisture over time.
df.n %>%
  filter(Node %in% Node[seq(1,max(Node),length=16)]) %>%
  ggplot(aes(x = Time, y = Moisture, color = -Depth, group = -Depth)) +
  geom_line() +
  scale_y_continuous(limits = c(0,0.45)) +
  geom_hline(yintercept = c(soil_para$thr,soil_para$ths), lty = 2, col = "red") + #mark theta_r and theta_s
  facet_wrap(~-Depth)
#this shows layers above water table declining in moisture and those below staying saturated
# Questions:
# - may be helpful to look more carefully at those near water table to see
# - uppermost layer seems to bounce off theta_r repeatedly - where is its new water coming from each time?

#Moisture over time above the water table and then just below it and at profile bottom
df.n %>%
  filter(Depth %in% seq(0,-40,-2)) %>%
  ggplot(aes(x = Time, y = Moisture, color = -Depth, group = -Depth)) +
  geom_line(lwd = 1.3, alpha = 0.5) +
  scale_y_continuous(limits = c(0,0.45)) +
  scale_color_viridis_c(option = "A") +
  geom_hline(yintercept = c(soil_para$thr,soil_para$ths), lty = 2, col = "red") + #mark theta_r and theta_s
  theme_bw()
#looks like even at 40 cm, the soil is saturated the whole time
#
#look again closer at near-saturation:
df.n %>%
  filter(Depth %in% seq(-30,-40,-1)) %>%
  ggplot(aes(x = Time, y = Moisture, color = -Depth, group = -Depth)) +
  geom_hline(yintercept = c(soil_para$thr,soil_para$ths), lty = 2, col = "red") + #mark theta_r and theta_s
  geom_line(lwd = 1.3, alpha = 0.5) +
  scale_y_continuous(limits = c(0,0.45)) +
  scale_color_viridis_c(option = "A") +
  theme_bw()
#looks like layers at 30-32 cm drain slightly, beyond this they seem to stay saturated.

#above I noted that surface layer keeps bouncing off of theta_r, being depleted and then slightly recharged, but I don't know where new water comes from.
# try to figure that out here
sapply(atm_bc_data, summary) #both rSoil and rRoot are active (T and E, respetively). they can be summed to equal PET
atm_bc_data$PET <- atm_bc_data$rSoil + atm_bc_data$rRoot
plot(df.n$Time[df.n$Node == 1], df.n$Flux[df.n$Node == 1], type = "l")
points(atm_bc_data$tAtm, atm_bc_data$PET, type = "l", col = "red")
#all this really shows is that the depletion (Flux > 0) corresponds to the hours when PET is active, which doesn't help

#see how flux relates to changes in moisture of first layer
df.n %>%
  filter(Node == 1) %>%
  mutate(moisture.chg = Moisture - lag(Moisture)
         #,last.flux = lag(Flux) #this doesn't make a cleaner graph
         ) %>%
  ggplot(aes(x=Flux, y=moisture.chg, color=Time)) +
  scale_color_viridis_c(option = "C") +
  geom_point() +
  theme_bw()
#there is a relation between moisture changes and flux, but it is not clean.
# - there's at least one more thing happening (capillary rise?), which I see when Flux = 0 and moisture.chg becomes positive, which shows
#     there is something else bringing water in.
df.n %>%
  filter(Node %in% seq(1,16, 3)) %>%
  group_by(Node) %>%
  mutate(moisture.chg = Moisture - lag(Moisture)
         #,last.flux = lag(Flux) #this doesn't make a cleaner graph
  ) %>%
  #ggplot(aes(x=Flux, y=moisture.chg, color=Node)) +
  filter(Time < 100) %>%
  ggplot(aes(x=Time, y=moisture.chg, color=Node)) +
  scale_color_viridis_c(option = "C") +
  geom_point() +
  geom_line() +
  facet_wrap(~Node) +
  theme_bw()
#my best guess is that it is capillary rise.
# new question: why does moisture withdrawal seem to only occur near surface? (very little negative fluctuation past depth ~10)
df.flux <- df.n %>%
  group_by(Depth) %>%
  summarize(flux.sum = sum(Flux))
df.flux$flux.sum[abs(df.flux$Depth) %in% c(0,10,20,40,50,100,200)] #these values go from deepest to surface
#plot these values
df.flux %>%
  ggplot(aes(x = Depth, y = flux.sum+1e-10)) +
  scale_y_log10() +
  geom_point()
#this looks odd. Flux due to evap can be seen clearly in 0-40 cm, but then why all these blips afterward?



#Head over time
# - this is hard to graph well because without log10 y axis, all that can be seen is the data at surface going wet to dry
#     but with log axis on then any non-saturated layers can't be displayed because log of negative numbers
# - do it both ways
df.n %>%
  filter(Node %in% Node[seq(1,max(Node),length=16)]) %>%
  ggplot(aes(x = Time, y = Head, color = -Depth, group = -Depth)) +
  geom_line() +
  coord_cartesian(ylim = c(-200,200)) +
  facet_wrap(~-Depth)
# this shows wet-dry fluctuations in top layer and not much else. can barely see water table change in lower layers
df.n %>%
  filter(Node %in% Node[seq(1,max(Node),length=16)]) %>%
  ggplot(aes(x = Time, y = Head, color = -Depth, group = -Depth)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~-Depth)
#this shows water table declining over time and blanks for the layer above water table becuase of log(negative number)

#this can rouhgly calculate water table depth over time
# where is head=0
df.wt <- df.n %>%
  group_by(Time) %>%
  summarize(wt.depth = Depth[which.min(abs(Head))])
plot(df.wt$Time, df.wt$wt.depth)
#which can be rouhgly confirmed by the positive head at some underwater node. use bottom nbode but could be any
points(df.n$Time[df.n$Depth == -200], df.n$Head[df.n$Depth == -200]-200, col = "red")



##########################
# not sure what else to learn here
# - the example is not relevant for me but was useful to understand the various input args and files, and to poke around to prove
#     some degree of understanding of the basic outputs.
# - next I need to run an example that will be relevant to me, whcih will be in "C:/Users/t/Documents/hydrusR/inst/examples/h1d_flow_example_tdb.R"
