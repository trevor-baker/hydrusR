#this example comes with hydrusR
# - the data it generates (see bottom) is very strange, making me wonder if there is an error in the hydrusR code, an error in Hydrus, or that
#     I don't understand what the example is trying to show
# - nonetheless, it does successfully run Hydrus for a single material profile with roots and ET.

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
# soil_para = list(thr = 0.045, ths = 0.43,
#                  Alfa = 0.145, n = 2.69, Ks = 29.7, l = 0.45)
soil_para = list(thr = c(0.045,0.1), ths = c(0.43,0.3),
                 Alfa = c(0.145,0.188), n = c(2.69,1.1), Ks = c(29.7,100), l = c(0.45,-1))

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
input_pet = TRUE
LAI = 4.0
et_rate = 0.6

## Boundary conditions inputs
const_botbc = TRUE
bot_bc_type = "flux"
const_botFlux = 0.0000 ##### in cm/hr

### Atmospheric top boundary conditions
### Time variable boundary conditions.
atm_bc_data = data.frame(tAtm = seq(time_step, endTime, time_step),
                         Prec = numeric(ntimesteps),
                         rSoil = numeric(ntimesteps),
                         rRoot = numeric(ntimesteps),
                         hCritA = rep(10000, ntimesteps),
                         rB = numeric(ntimesteps),
                         hB = numeric(ntimesteps),
                         ht = numeric(ntimesteps),
                         RootDepth = numeric(ntimesteps))


if(isTRUE(input_pet)) {
      atm_bc_data$rRoot = rep(LAI, nrow(atm_bc_data))
      atm_bc_data$rSoil = rep(et_rate/2, nrow(atm_bc_data))
} else {
      atm_bc_data$rRoot = rep(et_rate/2, nrow(atm_bc_data))
      atm_bc_data$rSoil = rep(et_rate/2, nrow(atm_bc_data))

}

################## for hourly time units
const_et = rep(et_rate, 365)
hourly_et =  et.hourly(Et.Daily = const_et)
hourly_et = hourly_et[rep(seq_len(nrow(hourly_et)), each = 1/time_step), ] ### for 0.25 time steps
hourly_et$rSoil = hourly_et$et/2
hourly_et$rRoot = hourly_et$et/2

if(isTRUE(input_pet)) {
      atm_bc_data$rRoot = rep(LAI, nrow(atm_bc_data))
      atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]
} else {
      atm_bc_data$rRoot = hourly_et$rRoot[1:nrow(atm_bc_data)]
      atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]

}

if(isTRUE(const_botbc)){
      atm_bc_data$hB = rep(0, nrow(atm_bc_data))
}

atm_bc_data = atm_bc_data[1:ntimesteps, ]

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
create.soil.profile(project.path = project_path,
                    profile.depth = profile_depth,
                    depth.vec = c(0,1,10,50,100,200),
                    mat = c(1,1,1,1,2,2),
                    head = -2,
                    obs.nodes = obs_nodes_all)


# #commented this out because it is called in create.soil.profile already. no need to duplicate.
# write.obs.nodes(project.path = project_path, obs.nodes = obs_nodes_all)

#set water table
write.ini.cond(project.path = project_path,
               wt.depth = initial_wtable)

#write root dist into Beta column - why not just have this and water table above be part of soil.profile fn?
write.root.dist(project.path = project_path,
                rdepth = rooting_depth,
                rBeta = 0.962)

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

#read outputs
#####################
library(ggplot2)
df.n <- read.nod_inf(project.path = project_path)
# - this data really looks werid but maybe the example is nonsense?

#Moisture over time.
df.n %>%
  filter(Node %in% Node[seq(1,max(Node),length=6)]) %>%
  ggplot(aes(x = Time, y = Moisture, color = -Depth, group = -Depth)) +
  geom_line() +
  facet_wrap(~-Depth)

#Head over time
df.n %>%
  filter(Node %in% Node[seq(1,max(Node),length=6)]) %>%
  ggplot(aes(x = Time, y = Head, color = -Depth, group = -Depth)) +
  geom_line() +
  facet_wrap(~-Depth)
