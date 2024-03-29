
source(here::here("R","00_load_functions.R"))
# here I just read in the first matrix given in the supplementary material to:
# Christian Dudel & Mikko Myrskylä (2017)
# Working Life Expectancy at Age 50 in the United States and the Impact of the Great Recession
# Demography https://link.springer.com/article/10.1007/s13524-017-0619-6


# get a transition matrix

TM_path <- here("Data","Dudel","Transition matrices","Pmat_b_f_1994.csv")
TM <- as.matrix(
		read.csv(TM_path,
				check.names = FALSE)
)
dim(TM)

# transpose for standard MC stuff.
# demographers do stuff transposed...
TM <- t(TM)

# make s4 transition matrix from markovchain package
mcEmpl <- new("markovchain", states = rownames(TM),
		byrow = TRUE, transitionMatrix = TM,
		name = "Empl")

# how many sequences should we generate?
N      <- 1e4

set.seed(1)
# each assuming a start in employment at age 50.
RTraj  <- replicate(N,
		rmarkovchain(n = 50, object = mcEmpl, t0 = "50::Employed", parallel = TRUE)
) 
RTraj                 <- rbind(rep("50::Employed",N), RTraj)
RTraj_clean           <- gsub(".*:","",RTraj)
rownames(RTraj_clean) <- 50:100

saveRDS(RTraj_clean, file = here::here("Data","Dudel","Rtraj_clean.rds"))
# ------------------
