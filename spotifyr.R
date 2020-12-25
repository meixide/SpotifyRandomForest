#devtools::install_github('charlie86/spotifyr')
#https://github.com/charlie86/spotifyr

library(dplyr)
library(spotifyr)
library(plotly)
library(ggplot2)
library(httpuv)

# engadir http://localhost:1410/ en Redirect URIs

id <- '60caf7d630d94336bee308f1d2b3e651'
secret <- '56b4cca9e8184787920b2e7158c3c5ff'

Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

access_token <- get_spotify_access_token()

my_id <- 'electrosystem32'



my_plistsi <- as.data.frame(get_my_playlists(limit=50))
my_plists=data.frame()
i=1
while(length(my_plistsi)!=0){

my_plists <- rbind(my_plists,my_plistsi)
i=i+1
my_plistsi=as.data.frame(get_my_playlists(limit=50, offset=((i-1)*50)))

}

minhas=my_plists[my_plists$owner.display_name=='electrosystem32',]

minhas$name
non=c("Johann Sebastian Bach - J.S. Bach: Concertos italiens","Play along sax","NADAL",
      "Reggae","Bach","Organ","Amancio Prada",
      "Deep","Tocable","Jazz","Soton","Covers","Para o loop",
      "Heavy","PianoCarlos", "OrqRomanticismo","Pink Floyd","Banda",
      "Folk","Rock","Level6","Slow rock","Piano")

ind_non=match(non,minhas$name)

uri_non=minhas[ind_non,11]

require('stringr')


for (i in 1:length(uri_non))
  uri_non[i]=str_sub(uri_non[i],-22,-1)


no_full=get_playlist_audio_features(
  'electrosystem32',
  uri_non,
  authorization = access_token
)

no=no_full[,c(6:17,which( colnames(no_full)=="track.name" ))]


  
  liked=get_playlist_audio_features(
    'electrosystem32',
    c('0c8fMM1PxkAeisOPo99ytt'),
    authorization = access_token
  )
  
  variables=c("y"  ,"danceability"  ,   "energy"   ,        "key"    ,         
              "loudness"     ,    "mode"         ,    "speechiness"  ,   
              "acousticness"  ,   "instrumentalness", "liveness"     ,   
              "valence"   ,       "tempo"     )
  
  liked_data=liked[,variables[-1]]
  
  
  
  
yes_full=get_playlist_audio_features(
  'electrosystem32',
  c('4Br0r0mKxy4IymbCq06Vg1'),
  authorization = access_token
)

yes=yes_full[,c(6:17,which( colnames(yes_full)=="track.name" ))]

no$y=rep(0,nrow(no))
yes$y=rep(1,nrow(yes))

training=as.data.frame(rbind(no,yes))

ind = sample(2, nrow(training), replace=TRUE, prob=c(0.8,0.2))
trainData = training[ind==1,]
testData = training[ind==2,]

colnames(trainData)




interese=trainData[,variables]
library("randomForest")

resp=factor(interese$y)



mod_rf = randomForest(x = interese[,variables[-1]],
                      y = resp)

predicions=predict(mod_rf, testData[,variables[-1]])

sum(testData$y==predicions)/nrow(testData) #what proportion well classified

mod_rf

varImpPlot(mod_rf)


predictions_ORIXINAIS=predict(mod_rf, liked_data)

URIS=liked$track.uri[predictions_ORIXINAIS=='1']


add_tracks_to_playlist(playlist_id = '1lONQ6kD9JphtJ4hZyH72J',
                       uris = URIS[201:length(URIS)]) #add in packs of 100



###


library('GGally')

ggpairs(yes,columns=2:11)


get_my_saved_tracks(
  limit = 50,
  offset = 0,
  authorization = access_token,
)

meta <- spotifyr::get_my_saved_tracks(limit = 50, offset = 0, include_meta_info = TRUE)

analysis=get_track_audio_analysis("7ycWLEP1GsNjVvcjawXz3z", authorization = access_token)

catalog=get_track("7ycWLEP1GsNjVvcjawXz3z", authorization = access_token)


