#my own example based on the one that comes with hydrusR ("h1dExample5")
#TDB, 2026-04-20
# - mine has two matls, no water table
# - nonetheless, it does successfully run Hydrus for a single material profile with roots and ET.

library(hydrusR); load_pkg()
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))

## Basic inputs
SpaceUnit = "cm" ## Space units
TimeUnit = "hours" ## time units
startTime = 0
endTime = 2400
time_step = 1

# soil inputs
profile_depth = 200
depth.vec = c(0,1,10,50,100,200)
mat.num =   c(1,1, 1, 1,  2,  2)
head.init = -1
rdepth = 100
rBeta = 0.962
subregions = seq(10,200,10)



#one matl
# soil_para = list(thr = 0.045, ths = 0.43,
#                  Alfa = 0.145, n = 2.69, Ks = 29.7, l = 0.45)
#two matl - give as list, one try per material, which will allow this structure to be kept whether passing
#             vG params or tables of psi,theta,K values
soil_para = list(list(thr = 0.045, ths = 0.43,
                      Alfa = 0.145, n = 2.69, Ks = 29.7, l = 0.45),
                 list(thr = 0.1, ths = 0.3,
                      Alfa = 0.154, n = 2.96, Ks = 92.7, l = -0.45)
                 )

# hydrus_path =  "C:/Program Files (x86)/PC-Progress/Hydrus-1D 4.xx"

project_name = "tdbExample1"
parent_dir = path.expand("~/temp")
project_path = path.expand(file.path(parent_dir, project_name))
dir.create(project_path, showWarnings = F)

##Process inputs
rwu = T   ###  rootwater uptake - passed to create.H1D.project()

##Profile/geometry inputs
obs_nodes_all = seq(20, profile_depth, by = 20)
nObsNodes = length(obs_nodes_all)
rooting_depth = 120

### Time inputs
total_timesteps = endTime/time_step
ntimesteps = length(1:total_timesteps)

## LAI and pet
et_rate = 0.4 #4 mm/day
trans.pc <- 1 #all is transipration from roots, none from soil

## Boundary conditions inputs
const_botbc = TRUE
bot_bc_type = "flux" #"head" #this determines which of the below. either 'flux' or 'head'
const_botFlux = 0.0 #in project units L/T
const_botHead = 0 #in project units L

### Time-variable Atmospheric top boundary conditions
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

#convert to hourly units
const_et = rep(et_rate, 365) #same daily value for 1 year
hourly_et =  pet.hourly(pet.daily = const_et) #hourly values for the year, varying throughout the day
hourly_et = hourly_et[rep(seq_len(nrow(hourly_et)), each = 1/time_step), ] #replicate for correct numbe rof timesteps
hourly_et$rSoil = hourly_et$pet*(1-trans.pc)
hourly_et$rRoot = hourly_et$pet*trans.pc
#transfer data into the atm_bc dataframe:
atm_bc_data$rRoot = hourly_et$rRoot[1:nrow(atm_bc_data)]
atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]

if(const_botbc){
  if(bot_bc_type == "flux"){
    atm_bc_data$rB <- const_botFlux
  } else {
    #else it is constant head
    atm_bc_data$hB <- const_botHead
  }
}


# #this trims to the correct number of rows for the simulation being run
# atm_bc_data = atm_bc_data[1:which(atm_bc_data == endTime), ]
# #redundant because this is how $tAtm was set up to start



#### Creates a blank hydrus project with three files: "HYDRUS1D.DAT", "SELECTOR.IN" and "DESCRIPT.TXT"
create.H1D.project(project.name = project_name,
                   parent.dir = parent_dir,
                   description = paste("TDB MSc:", project_name),
                   overwrite = T,
                   processes = c(WaterFlow = T, RootWaterUptake = rwu),
                   units = c(TimeUnit = TimeUnit, SpaceUnit = SpaceUnit),
                   profile = c(SubregionNumbers = length(subregions)),
                   setup = c(InitialCondition = 0),
                   geometry = c(ProfileDepth = profile_depth,
                                NumberOfNodes = length(depth.vec),
                                ObservationNodes = nObsNodes),
                   times = list(time.range1 = 0, time.range2 = 2400,
                                dt = 1e-3, dtMin = 1e-6, dtMax = 1,
                                DMul1 = 0.7, DMul2 = 1.3, ItRange1 = 3, ItRange2 = 7,
                                print.step = 10),
                   sim = list(MaxIt = 100, TolTh = 0.001, TolH = 1,
                              TopInf = TRUE, BotInf = FALSE, WLayer = TRUE,
                              KodTop = -1, KodBot = -1, InitCond = FALSE,
                              qGWLF = FALSE, FreeD = TRUE, SeepF = FALSE,
                              DrainF = FALSE, hSeep = 0),
                   root = list(model = 0, comp = 1,
                               P0 = -10, P2H = -200, P2L = -800, P3 = -8000,
                               POptm = -25, r2H = 0.5, r2L = 0.1,
                               root_depth = rooting_depth,
                               rBeta = 0.962),
                   soil.para = soil_para,
                   soil.model = 0,
                   soil.hys = 0,
                   soil.slope = 0)

### create the soil profile (PROFILE.DAT) info
create.soil.profile(project.path = project_path,
                    profile.depth = profile_depth,
                    depth.vec = depth.vec,
                    root.depth = rooting_depth,
                    rBeta = rBeta,
                    mat = mat.num,
                    head = head.init,
                    obs.nodes = obs_nodes_all)


# #set water table - I don't need this for my work
# write.ini.cond(project.path = project_path,
#                wt.depth = NULL) #make sure this fn will work even if set to NULL

write.bottom.bc(constant.bc = TRUE,
                bc.type = bot_bc_type,
                bc.value = const_botFlux,
                project.path = project_path)

write.atmosph.in(project.path = project_path,
                 maxAL = 2000,
                 deltaT = time_step,
                 atm.bc.data = atm_bc_data[1:2000,])


#to do:
# - write a wrapper fn in IEGsoil to contain all of these and transfer the correct data.
# -- prior to this just test simple loop of 2 simulations.
#
#run.H1D questions
# - understand what beginT, endT, deltaT do
# -- why aren't these taken from the project files created already?
# -- are they used only for the print times and the lines of the BC files? or do they control the simulation timesteps?
# - do bot.bc.type and value need to agree with KodBot etc. in SELECTOR.IN?


##### Default hydrus path in Windows

run.H1D.simulation(project.path = project_path,
                   profile.depth = profile_depth,
                   beginT = startTime, endT = endTime, deltaT = time_step,
                   bot.bc.type = bot_bc_type,
                   bot.bc.value = const_botFlux,
                   const.bot.bc = TRUE,
                   atm.bc.data = atm_bc_data,
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
