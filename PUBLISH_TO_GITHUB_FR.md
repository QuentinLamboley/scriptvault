# Publier ScriptVault sur GitHub

Ce dossier est déjà organisé comme un dépôt GitHub : le fichier `DESCRIPTION` et le dossier `R/` sont à la racine. Ne déposez **pas** l'archive ZIP elle-même sur GitHub : décompressez-la d'abord, puis publiez le contenu du dossier.

## Méthode la plus simple : GitHub Desktop ou interface web

1. Créez un nouveau dépôt GitHub nommé **`scriptvault`**.
2. Choisissez une visibilité **publique** si vous souhaitez que les chercheurs puissent l'installer, ou **privée** pour une phase de test.
3. Ne cochez pas l'option qui crée automatiquement un README, une licence ou un `.gitignore` : ils existent déjà dans ce dossier.
4. Décompressez `ScriptVault_GitHub_v0.1.0.zip`.
5. Dans GitHub Desktop, choisissez **Add existing repository**, sélectionnez le dossier extrait, puis **Publish repository**.
6. Sinon, dans l’interface GitHub, ouvrez votre dépôt puis **Add file → Upload files** et envoyez le contenu du dossier extrait, y compris `.github/`.

## Méthode terminal Windows

Dans PowerShell ou Git Bash, placez-vous dans le dossier extrait puis remplacez `VOTRE_COMPTE_GITHUB` :

```powershell
git init
git add .
git commit -m "feat: initial public release of ScriptVault v0.1.0"
git branch -M main
git remote add origin https://github.com/VOTRE_COMPTE_GITHUB/scriptvault.git
git push -u origin main
```

## Après la première publication

1. Dans GitHub, ouvrez **Settings → Actions → General** et autorisez les workflows si GitHub vous le demande.
2. Vérifiez l'onglet **Actions** : `R-CMD-check` testera le package sur Linux, Windows et macOS à chaque modification.
3. Dans `README.md`, remplacez `VOTRE_COMPTE_GITHUB` par votre identifiant GitHub, uniquement dans la commande d'installation.
4. Dans `.github/ISSUE_TEMPLATE/config.yml`, remplacez `OWNER` par votre identifiant GitHub.
5. Pour créer une release téléchargeable, créez un tag GitHub commençant par `v`, par exemple `v0.1.0`. Le workflow générera automatiquement `scriptvault_0.1.0.tar.gz` et l'ajoutera à la release.

## Installation pour les futurs utilisateurs

Une fois publié, ils pourront installer la version GitHub avec :

```r
install.packages("remotes")
remotes::install_github("VOTRE_COMPTE_GITHUB/scriptvault")
```

Puis :

```r
library(scriptvault)
sv_init()
sv_watch()
```
