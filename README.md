
# Comment utiliser ce serveur ?

## Compilation

Mieux vaut se munir de [Nix](https://nixos.org) et des [flakes](https://www.tweag.io/blog/2020-05-25-flakes/).
Sinon, `cabal` peut permettre de `build` le serveur également.

> TL;DR `nix run github:HiiGHoVuTi/integration-course`

### Obtenir la source

```
git clone git@github.com:HiiGHoVuTi/integration-course.git
```

### Lancer le serveur

Depuis votre machine munie de Nix,
```
nix run github:HiiGHoVuTi/integration-course
```

Ou si vous avez obtenu le code source,
```
nix run
```
Et sans Nix,
```
cabal run
```

### Compiler le serveur

Après avoir obtenu le code source,
```
nix build
```

ou sans nix, `cabal build`.

## Utilisation

### API

Voir la définition du type [`API`](https://github.com/HiiGHoVuTi/integration-course/blob/main/app/API.hs).

### Ligne de commandes

Lorsque le serveur est lancé, vous pouvez entrer l'une des commandes suivantes:
- `quit`/`exit` éteint le serveur
- `ready` indique que la partie peut commencer, `GET /ready` renverra `true`
- `claims` donne la liste des réclamations faites par les joueurs
- `validate <n>` permet de valider l'une d'entre elles
- `refuse <n>` permet de jeter l'une d'entre elles
