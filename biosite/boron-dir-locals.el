(
 (nil . ((projectile-project-compilation-dir . "./build")
         (projectile-project-compilation-cmd . "ninja")

         ;; Include common in file listings
         (projectile-hg-command .  "hg locate -f -0 -I . && hg locate -R common -f -0 -I common/")
         )
      )
 )
