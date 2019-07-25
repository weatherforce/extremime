# Projet EXTREMIME
Procédure d'installation de l'outil intégré de traitement et d'analyse des séries temporelles

## Informations générales et prérequis techniques 

L'outil développé est construit sur la base du logiciel libre, multi plateforme *[Docker](https://fr.wikipedia.org/wiki/Docker_(logiciel))*. 

*Docker* permet la construction d'environnement, leur distribution et leur exécution, sur des systèmes d'exploitations différents (Linux, Windows ou Mac).

Le logiciel Docker doit être installé sur l'ordinateur sur lequel l'outil est utilisé, et fonctionner correctement.


## Installation et lancement de *Docker*

### Ubuntu & Fedora

Le logiciel *Docker* fait partie des dépôts officiels des distributions Ubuntu et Fedora. Pour l'installer, il suffit donc d'installer le paquet **docker**. 

Une procédure alternative est mentionnée sur le site officiel de Docker :
* [Procédure pour Ubuntu](https://docs.docker.com/install/linux/docker-ce/ubuntu/#install-docker-ce)
* [Procédure pour Fédora](https://docs.docker.com/install/linux/docker-ce/fedora/#install-docker-ce)


### Mac
Télécharger la dernière version stable de "Docker Community Edition" pour Mac [via ce lien ](https://docs.docker.com/docker-for-mac/release-notes/) puis :
1. Cliquez sur *Docker.dmg* pour lancer l'installateur et enfin flissez-déposez l'icone de Docker dans votre répertoire *"Applications"*.
2. Cliquez sur *Docker.app* dans votre répertoire *Applications* pour lancer *Docker*.
Dès que le logo de Docker, une baleine, est visible dans la barre de status, cela signifie que Docker fonctionne.

La procédure d'installation officielle est disponible, en anglais, sur le site de *Docker* [via ce lien](https://docs.docker.com/docker-for-mac/install/#install-and-run-docker-desktop-for-mac).

### Windows

Télécharger la dernière version stable de "Docker Community Edition" pour Windows [via ce lien ](https://docs.docker.com/docker-for-windows/release-notes/) puis :
1. Cliquez sur l'installateur Docker Desktop for Windows pour lancer l'installation et suivez le processus d'installation étapes par étapes.
3. Pour démarrer Docker, recherchez Docker dans le menu Demarrer, et cliquez sur "Docker Desktop for Windows".

La procédure d'installtion officielle est disponible, en anglais, sur le site de *Docker* [via ce lien](https://docs.docker.com/docker-for-windows/install/#install-docker-desktop-for-windows-desktop-app).

## Vérification du fonctionnement de *Docker*
Lorsque *Docker* est en cours d'exécution sur le poste de travail, lancer la commande suivante dans un Terminal :

	docker run hello-world:latest
Les informations suivantes doivent s'afficher à l'écran :
    
    Unable to find image 'hello-world:latest' locally
    latest: Pulling from library/hello-world
    1b930d010525: Pull complete
    Digest: sha256:92695bc579f31df7a63da6922075d0666e565ceccad16b59c3374d2cf4e8e50e
    Status: Downloaded newer image for hello-world:latest

    Hello from Docker!
    This message shows that your installation appears to be working correctly.

    To generate this message, Docker took the following steps:
     1. The Docker client contacted the Docker daemon.
     2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
        (amd64)
     3. The Docker daemon created a new container from that image which runs the
        executable that produces the output you are currently reading.
     4. The Docker daemon streamed that output to the Docker client, which sent it
        to your terminal.

    To try something more ambitious, you can run an Ubuntu container with:
     $ docker run -it ubuntu bash

    Share images, automate workflows, and more with a free Docker ID:
     https://hub.docker.com/

    For more examples and ideas, visit:
     https://docs.docker.com/get-started/


## Téléchargement et lancement de l'outil de traitement

Lorsque *Docker* est en cours d'exécution sur le poste de travail lancer la commande suivante dans un Terminal, en étant, au préalable, positionné dans le répertoire contenant ses données :

	docker pull weatherforce/extremime:latest && docker run -p 8888:8888 -v "$PWD":/home/jovyan/work weatherforce/extremime:latest

Lors de la première exécution, cette commande va procéder au téléchargement de l'environnement de travail, puis à son lancement sur le poste.

Lorsque l'environnement est prêt à être accédé, les lignes suivantes sont affichées dans le terminal :
	
    Copy/paste this URL into your browser when you connect for the first time,
    to login with a token:
        http://localhost:8888/?token=...

Copier coller l'adresse affichée dans le terminal, dans un navigateur internet, pour lancer l'outil de traitement.
