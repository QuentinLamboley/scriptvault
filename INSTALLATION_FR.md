# Installation locale de ScriptVault (Windows, macOS, Linux)

## Installation simple

1. Décompressez le dossier de livraison.
2. Ouvrez R ou RStudio.
3. Placez-vous dans le dossier qui contient `scriptvault_0.1.0.tar.gz`.
4. Exécutez :

```r
install.packages(c("DBI", "RSQLite", "digest", "filelock", "fs", "jsonlite", "later", "rstudioapi"))
install.packages("scriptvault_0.1.0.tar.gz", repos = NULL, type = "source")
library(scriptvault)
```

Le package ne dépend ni de Git, ni de GitHub, ni d'un compte en ligne. Il est écrit intégralement en R : il ne contient pas de code C/C++ à compiler.

## Premier projet

Dans le projet R à versionner :

```r
library(scriptvault)
sv_init()
sv_watch(interval = 1)
sv_install_project_hook(interval = 1)
```

- `sv_init()` crée `.scriptvault/` dans le projet et archive l'état initial des scripts.
- `sv_watch()` archive automatiquement les changements détectés pendant la session R en cours.
- `sv_install_project_hook()` ajoute un petit bloc explicite dans `.Rprofile` afin que la surveillance démarre à chaque ouverture du projet.

## Utilisation dans RStudio

Après l'installation, redémarrez RStudio puis ouvrez le menu **Addins** :

- **Snapshot active script** : archive immédiatement le script actuellement ouvert.
- **Save and snapshot active script** : enregistre le document actif puis l'archive immédiatement.
- **Start / Stop ScriptVault watcher** : démarre ou arrête la surveillance automatique.

La surveillance repose sur la détection de fichiers écrits sur le disque. R ne dispose pas d'un événement universel, valable dans tous les éditeurs, qui intercepte individuellement chaque `Ctrl+S`. Pour une action exactement couplée à l'enregistrement dans RStudio, utilisez l'addin « Save and snapshot active script » et attribuez-lui un raccourci dans les préférences RStudio.

## Sauvegarde

Sauvegardez toujours le dossier de projet **avec** le dossier caché `.scriptvault/`. C'est ce dernier qui contient l'historique, les branches et les traces de reproductibilité.

## Installation depuis GitHub

Après publication du dépôt, l’installation peut se faire directement depuis GitHub :

```r
install.packages("remotes")
remotes::install_github("VOTRE_COMPTE_GITHUB/scriptvault")
library(scriptvault)
```

Remplacez `VOTRE_COMPTE_GITHUB` par l’identifiant du propriétaire du dépôt.
