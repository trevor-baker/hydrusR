#my own example based on the one that comes with hydrusR ("h1dExample5")
#TDB, 2026-04-20
# - mine has two matls, no water table
# - nonetheless, it does successfully run Hydrus for a single material profile with roots and ET.

library(hydrusR); load_pkg()
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))


#set directories
project_name = "tdbExample1"
parent_dir = path.expand("~/temp")
project_path = path.expand(file.path(parent_dir, project_name))
dir.create(project_path, showWarnings = F)

# time and units
SpaceUnit = "cm" ## Space units
TimeUnit = "hours" ## time units
startTime = 0
endTime = 2400
time_step = 1

##Simulation settings
rwu = TRUE   #rootwater uptake - passed to create.H1D.project()
maxIt <- 100


## atmosphere
PET_mmd <- 4 #4 mm/day
trans_pc <- 1 #all is transipration from roots, none from soil
Prec_mmd <- 0 # mm/day

# Boundary conditions inputs
atmos_bc <- TRUE #has atmosphere BC
FreeD <- TRUE #free draining at bottom

#if freeD is TRUE, then these for bottom are ignored:
const_botbc = TRUE
bot_bc_type = "flux" #"head" #this determines which of the below. either 'flux' or 'head'
const_botFlux = 0.0 #in project units L/T
const_botHead = 0 #in project units L

#if atmos = TRUE, then these for top are ignored:
const_topbc <- TRUE
top_bc_type <- "flux"
const_topFlux <- 0 #in project units L/T
const_tophead <- 0 #in project units L
hCritS <- 0


# soil inputs
profile_depth = 200
depth_vec = c(0,1,10,50,100,200)
mat_num =   c(1,1, 1, 1,  2,  2)
head_init = -1
root_depth = 100
rBeta = 0.962
subregions = seq(10,200,10)
obs_nodes = seq(20, profile_depth, by = 20)

# - soil parameters:
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






##Profile/geometry inputs
#nObsNodes = length(obs_nodes)


### Time inputs
#total_timesteps = endTime/time_step
#ntimesteps = length(1:total_timesteps)



# ### Time-variable Atmospheric top boundary conditions
# atm_bc_data = data.frame(tAtm = seq(time_step, endTime, time_step), #time, same units as project
#                          Prec = numeric(ntimesteps), #precipitation in L/T units
#                          rSoil = numeric(ntimesteps), #potential evap rate in L/T units
#                          rRoot = numeric(ntimesteps), #potential transpiration in L/T. rRoot+rSoil = PET
#                          hCritA = rep(10000, ntimesteps), #Absolute value of the minimum allowed pressure head at the soil surface [L]. unsure what effect this has, if any, so long as it is sufficiently dry. Hydrus manual shows 1e6 not 1e4 like here.
#                          # per manual - this must be at least theta_r+0.005 for solution stability. also must be drier than P3 value in root stress fn, which is
#                          # typically between 8000-16000, depending on reference crop settings
#                          rB = numeric(ntimesteps), #flux rate at bottom, in L/T
#                          hB = numeric(ntimesteps), #pressure head at bottom, in L units
#                          ht = numeric(ntimesteps), #pressure head at top, in L units
#                          RootDepth = numeric(ntimesteps)) #I don't think this is necessary for this dataset.
#
# #convert to hourly units
# const_et = rep(et_rate, 365) #same daily value for 1 year
# hourly_et =  pet.hourly(pet.daily = const_et) #hourly values for the year, varying throughout the day
# hourly_et = hourly_et[rep(seq_len(nrow(hourly_et)), each = 1/time_step), ] #replicate for correct numbe rof timesteps
# hourly_et$rSoil = hourly_et$pet*(1-trans.pc)
# hourly_et$rRoot = hourly_et$pet*trans.pc
# #transfer data into the atm_bc dataframe:
# atm_bc_data$rRoot = hourly_et$rRoot[1:nrow(atm_bc_data)]
# atm_bc_data$rSoil = hourly_et$rSoil[1:nrow(atm_bc_data)]
#
# if(const_botbc){
#   if(bot_bc_type == "flux"){
#     atm_bc_data$rB <- const_botFlux
#   } else {
#     #else it is constant head
#     atm_bc_data$hB <- const_botHead
#   }
# }
#
#
# # #this trims to the correct number of rows for the simulation being run
# # atm_bc_data = atm_bc_data[1:which(atm_bc_data == endTime), ]
# # #redundant because this is how $tAtm was set up to start



#get arguments for creating project from boundary condition inputs
bc.args <- data.frame(TopInf = NA, KodTop = NA_integer_,
                      BotInf = NA, KodBot = NA_integer_,
                      WLayer = NA,
                      FreeD = FreeD)
bc.args$TopInf <- atmos | !const_topbc  #is the top BC variable (TRUE) or constant (FALSE) - atmos gets entered here as FALSE even though it can vary
bc.args$BotInf <- if(FreeD){ FALSE } else { #is the bottom BC variable (TRUE) or constant (FALSE) - FreeD gets set as FALSE by Hydrus
  if(const_botbc){ FALSE } else {TRUE } }

bc.args$KodTop <- if(atmos){ -1 } else { #this is entered as -1 (constant) for simulations with atmos data, even if the flux (e.g. P or ET) is variable over time.
  if(const_topbc){
    #constants are (1), flux is -1 and head is +1
    if(top_bc_type == "flux"){ -1 } else { +1 }} else {
      #constants are (3), flux is -3 and head is +3
      if(top_bc_type == "flux"){ -3 } else { +3 }
    }
}
bc.args$KodBot <- if(FreeD){ -1 } else { #this is entered as -1 (constant) for simulations with free-draining profiles, even if the flux will vary as the profile drains.
  if(const_botbc){
    #constants are (1), flux is -1 and head is +1
    if(bot_bc_type == "flux"){ -1 } else { +1 }} else {
      #constants are (3), flux is -3 and head is +3
      if(bot_bc_type == "flux"){ -3 } else { +3 }
    }
}
bc.args$WLayer <- if(hCritS > 0){ TRUE } else { FALSE } #if positive head allowed at surface, then a water layer can build up without running off

#### Creates a blank hydrus project with three files: "HYDRUS1D.DAT", "SELECTOR.IN" and "DESCRIPT.TXT"
create.H1D.project(project.name = project_name,
                   parent.dir = parent_dir,
                   description = paste("TDB MSc:", project_name),
                   overwrite = T,
                   processes = c(WaterFlow = TRUE, RootWaterUptake = rwu),
                   units = c(TimeUnit = TimeUnit, SpaceUnit = SpaceUnit),
                   profile = c(SubregionNumbers = length(subregions)),
                   setup = c(InitialCondition = 0),
                   geometry = c(ProfileDepth = profile_depth,
                                NumberOfNodes = length(depth.vec),
                                ObservationNodes = length(obs_nodes)),
                   times = list(time.range1 = startTime, time.range2 = endTime,
                                dt = 1e-3, dtMin = 1e-6, dtMax = 1,
                                DMul1 = 0.7, DMul2 = 1.3, ItRange1 = 3, ItRange2 = 7,
                                print.step = 10),
                   sim = list(MaxIt = 100, TolTh = 0.001, TolH = 1,
                              TopInf = bc.args$TopInf, BotInf = bc.args$BotInf, WLayer = bc.args$WLayer,
                              KodTop = bc.args$KodTop, KodBot = bc.args$KodBot,
                              InitCond = FALSE, qGWLF = FALSE,
                              FreeD = FreeD,
                              SeepF = FALSE, DrainF = FALSE, hSeep = 0),
                   root = list(model = 0, comp = 1,
                               P0 = -10, P2H = -200, P2L = -800, P3 = -8000,
                               POptm = -25, r2H = 0.5, r2L = 0.1,
                               root_depth = root_depth,
                               rBeta = rbeta),
                   soil.para = soil_para,
                   soil.model = 0,
                   soil.hys = 0,
                   soil.slope = 0)

### create the soil profile (PROFILE.DAT) info
create.soil.profile(project.path = project_path,
                    profile.depth = profile_depth,
                    depth.vec = depth.vec,
                    root.depth = root_depth,
                    rBeta = rBeta,
                    mat = mat.num,
                    head = head.init,
                    obs.nodes = obs_nodes)


# #set water table - I don't need this for my work
# write.ini.cond(project.path = project_path,
#                wt.depth = NULL) #make sure this fn will work even if set to NULL

df.atm <- prep.H1D.climate(project.path = project_path,
                           TimeUnit = TimeUnit, SpaceUnit = SpaceUnit,
                           endTime = endTime,
                           time_values = c(1200,2400),
                           Prec_values = c(0, 0.01),
                           PET_values =  c(0.01, 0),
                           trans.pc = 0.8)

create.bc(project.path = project_path,
          atmos = TRUE,
          atmos.df = df.atm,
          hCritS = 0,
          top.bc.type = NULL,
          top.bc.value = NULL,
          freeD = TRUE,
          bot.bc.type = NULL,
          bot.bc.value = NULL)

#these are now in create.bc()
# write.bottom.bc(constant.bc = TRUE,
#                 bc.type = bot_bc_type,
#                 bc.value = const_botFlux,
#                 project.path = project_path)
#
# write.atmosph.in(project.path = project_path,
#                  maxAL = 2000,
#                  deltaT = time_step,
#                  atm.bc.data = atm_bc_data[1:2000,])


#to do:
# - write a wrapper fn in IEGsoil to contain all of these and transfer the correct data.
# -- prior to this just test simple loop of 2 simulations.
# -- prior to this, try to simply replicate a freeD profile with no ATMOS data, then with constant (then variable) ATMOS data.
# --- replicate the data prep, and then check results against GUI version of saem setup.
# --- would be nice to test others, but I know all my work in near future will be freeD with and without Prec and ET.
#
# - still stuck on PrintTimes and atm times. these are tied together somehow. they are separate. not sure how I'm passing print times right now.
#



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
#run.H1D questions (leaving these for now as they all seem to apply to long simulations only)
# - understand what beginT, endT, deltaT do
# -- why aren't these taken from the project files created already?
# -- are they used only for the print times and the lines of the BC files? or do they control the simulation timesteps?
# - do bot.bc.type and value need to agree with KodBot etc. in SELECTOR.IN?


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
