## SDL2

Avant ```stack build```, exécuter les deux commandes suivantes pour que SDL2 et SDL2-ttf s'installent correctement (instructions : https://www.reddit.com/r/haskellgamedev/comments/4jpthu/windows_sdl2_is_now_almost_painless_via_stack/). 



* ``` stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2```
* ``` stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2_ttf```

Sur windows, la dernière version de SDL2 ne fonctionne pas, une solution est de faire (voir https://github.com/haskell-game/sdl2/issues/277#issuecomment-1339145721) : 

``` 
$ stack exec -- curl -O https://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-SDL2-2.0.14-2-any.pkg.tar.zst
$ stack exec -- pacman -U mingw-w64-x86_64-SDL2-2.0.14-2-any.pkg.tar.zst
```

## Workarounds pour windows

L'erreur 
``` commitAndReleaseBuffer: invalid argument (invalid character) ``` provient d'un problème d'encodage. Pour régler le problème, on peut utilise ```chcp.com 65001 ```