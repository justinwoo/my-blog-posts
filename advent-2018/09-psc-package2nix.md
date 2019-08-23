So while Psc-Package is a nice tool to figure out our dependencies from a package set and a list of direct dependencies, it leaves a lot to be desired when we think about how packages are downloaded. Do we need it to have git fetch our dependencies whenever we don't have them already installed?

And while many will say that Psc-Package indeed shouldn't use git to fetch every dependency, I think many will point out that even if you tried to store these, you'd need to verify the contents somehow, never mind the endless hell that is trying to figure out where you can actually write files on a user's system globally without trouble (well, ignoring all of the unprincipled approaches of shitting files into a user's HOME directory somewhere).

So what if instead of trying to make yet another shitty solution, we can use a solution that does try to do the right thing? If we use Nix for this, we can write derivations for how the dependencies are to be fetched, and those will be hashed. When these derivations are run, Nix will create store entries for these derivations. How do we generate this from a psc-packages.json file though?

## "Nix-ify your Psc-Package dependencies"

In this post, I talked about how the psc-package2nix tool works, by using the package set and the direct dependencies to generate derivations for each package with the correct sha256 hash needed to verify the contents of the dependency sources.

<https://qiita.com/kimagure/items/85a64437f9af78398638>

This also goes into how dependencies are solved for in Psc-Package by traversing the transitive dependencies.

With the Psc-Package2Nix tool, we can now leverage Nix to download and cache dependencies for our projects, where only in the final step, we can use a small derivation to copy the dependencies from the store into the appropriate directories in `.psc-package/{set}/{package}/{version}`. This means that between projects, changes in package sets, and re-installs, we don't have to download any dependencies redundantly, and can instead have Nix handle the details of when new dependencies need to be downloaded.
