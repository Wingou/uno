# Get Started

## GitHub :
Ajouter un repo /finance

https://github.com/Wingou/uno

## GIT : Install
https://git-scm.com/download/win

## NODE.JS : Install
https://nodejs.org/en/download/

## NPM : Install
$> npm install npm@latest -g

## Create-Elm-App : Install
$> npm install create-elm-app -g

## Create Finance

sur \Dev

Créer l'appli, MonApp
$> create-elm-app MonApp

Puis, copier-le dans le dossier de votre appli \uno

Ensuite, supprimer le dossier \MonApp

$> cd finance

$> elm-app start

Pour importer les lib :
par exemple :
$> elm-app install elm/random 



## Calculs dimension Sprite

|Horizontal Sprite|	Vertical Sprite|	Rapport Sprite|	horizontal Carte|	vertical Carte|	rapport Carte|	Intersection|
|---|---|---|---|---|---|---|
|L connu	|h|	R connu|	x|	y|	r connu|	i|
|3029|	1454|	2,083218707|	171|	256|	1,497076023|	29|
|1400|	2916,50619|	2,083218707|	79,03598547|	118,3228788|	1,497076023|	13,40376362|
|1400|	2917|	|	79|	118|	|	13|
|L	|h=R.L| R|x=(L-16i)/15|	y=r.x|	r=y/x|	i=(L.(3-r.R))/(2.R.(9-8r)) |

