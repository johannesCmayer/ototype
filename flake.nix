{
  description = "An empty flake template that you can adapt to your own environment";

  # Flake inputs
  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";

  # Flake outputs
  outputs =
    inputs:
    let
      # The systems supported for this flake
      supportedSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
        "aarch64-linux" # 64-bit ARM Linux
        "x86_64-darwin" # 64-bit Intel macOS
        "aarch64-darwin" # 64-bit ARM macOS
      ];

      # Helper to provide system-specific attributes
      forEachSupportedSystem =
        f:
        inputs.nixpkgs.lib.genAttrs supportedSystems (
          system:
          f {
            pkgs = import inputs.nixpkgs { inherit system; };
          }
        );
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
          default = pkgs.mkShell rec {
            # The Nix packages provided in the environment
            # Add any you need here
            packages = with pkgs; [
              sbcl
              portaudio
              openssl
              
              makeWrapper
            ];

            # Set any environment variables for your dev shell
            env = { };

            # Add any shell logic you want executed any time the environment is activated
            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath packages}:$LD_LIBRARY_PATH
              # MEGA HACK
              makeWrapper /home/johannes/projects/system-wide-stt/system-wide-stt-bin \
                          /home/johannes/projects/system-wide-stt/system-wide-stt \
                          --set LD_LIBRARY_PATH "$LD_LIBRARY_PATH"
            '';
          };
        }
      );
    };
}


# How can I make it such that this LD_LIBRARY_PATH is actually baked into a safe-list-and-die image? I need that when running the binary from another directory than this flake.

# To bake the `LD_LIBRARY_PATH` into a binary that can run from any directory, you have a few options. Here are the most common approaches:

# ## Option 1: Use `pkgs.runCommand` with wrapper script

# ```nix
# outputs = inputs:
#   let
#     # ... your existing code ...
#   in
#   {
#     packages = forEachSupportedSystem ({ pkgs }: {
#       default = pkgs.runCommand "my-app" {
#         buildInputs = with pkgs; [ sbcl portaudio openssl ];
#       } ''
#         mkdir -p $out/bin
        
#         # Create wrapper script that sets LD_LIBRARY_PATH
#         cat > $out/bin/my-app << EOF
#         #!/bin/bash
#         export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.portaudio pkgs.openssl ]}:\$LD_LIBRARY_PATH"
#         exec ${pkgs.sbcl}/bin/sbcl "\$@"
#         EOF
        
#         chmod +x $out/bin/my-app
#       '';
#     });
    
#     # Keep your existing devShells
#     devShells = forEachSupportedSystem ({ pkgs }: {
#       # ... your existing devShell ...
#     });
#   };
# ```

# ## Option 2: Use `pkgs.writeShellApplication` (cleaner)

# ```nix
# packages = forEachSupportedSystem ({ pkgs }: {
#   default = pkgs.writeShellApplication {
#     name = "my-app";
#     runtimeInputs = with pkgs; [ sbcl portaudio openssl ];
#     text = ''
#       export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.portaudio pkgs.openssl ]}:$LD_LIBRARY_PATH"
#       exec sbcl "$@"
#     '';
#   };
# });
# ```

# ## Option 3: Use `wrapProgram` for existing binaries

# If you have an actual binary to wrap:

# ```nix
# packages = forEachSupportedSystem ({ pkgs }: {
#   default = pkgs.runCommand "wrapped-binary" {
#     buildInputs = [ pkgs.makeWrapper ];
#   } ''
#     mkdir -p $out/bin
#     makeWrapper ${pkgs.sbcl}/bin/sbcl $out/bin/my-app \
#       --set LD_LIBRARY_PATH "${pkgs.lib.makeLibraryPath [ pkgs.portaudio pkgs.openssl ]}"
#   '';
# });
# ```

# After adding one of these, you can build with `nix build` and the resulting binary in `./result/bin/` will have the library paths baked in and work from any directory.

# Is it normal that the exported lisp does try to load the libs at runtime?

# Yes, it's completely normal for exported/compiled Lisp binaries to try to load libraries at runtime, especially when using FFI (Foreign Function Interface) to interact with C libraries like PortAudio and OpenSSL.

# Here's why this happens:

# ## Dynamic vs Static Linking

# 1. **Dynamic linking** (most common): The Lisp binary contains references to shared libraries (`.so` files on Linux, `.dylib` on macOS) that must be found and loaded when the program starts
# 2. **Static linking** (less common): Libraries are embedded directly into the binary

# ## Common Lisp FFI behavior

# When you use libraries like:
# - **PortAudio** via `cl-portaudio` 
# - **OpenSSL** via `cl+ssl` or similar
# - **SQLite** via `cl-sqlite`

# These typically use `cffi` (Common Foreign Function Interface) which:
# - Loads shared libraries at runtime using `dlopen()` 
# - Looks for libraries in standard system paths + `LD_LIBRARY_PATH`
# - Fails if libraries aren't found

# ## Solutions in order of preference:

# 1. **Nix wrapper** (what you're doing) - sets `LD_LIBRARY_PATH`
# 2. **RPATH embedding** - bake library paths directly into the binary:
#    ```nix
#    shellHook = ''
#      export LDFLAGS="-Wl,-rpath,${pkgs.lib.makeLibraryPath packages}"
#    '';
#    ```
# 3. **Static compilation** - some Lisp implementations support this but it's complex
# 4. **Container/AppImage** - bundle everything together

#         Your Nix approach is actually the cleanest solution for this problem! The wrapper ensures your binary can find its dependencies regardless of where it runs.


# But how can I build a nix application such that it will run on any OS, and any linux? Would I need to manually bundle the dynamic libs?

# Unfortunately, you **cannot** build a single Nix application that runs on "any OS and any Linux" due to fundamental compatibility issues. Here's why and what your options are:

# ## The Problem

# 1. **Different architectures**: x86_64 vs ARM vs others
# 2. **Different operating systems**: Linux vs macOS vs Windows  
# 3. **Different libc implementations**: glibc vs musl vs others
# 4. **Different kernel versions and ABIs**

# Nix builds are tied to specific system tuples like `x86_64-linux`, `aarch64-darwin`, etc.

# ## Your Options (in order of practicality):

# ### 1. **Static Linking** (Best for portability)
# ```nix
# packages = forEachSupportedSystem ({ pkgs }: {
#   static = pkgs.pkgsStatic.runCommand "my-static-app" {
#     buildInputs = with pkgs.pkgsStatic; [ sbcl portaudio openssl ];
#   } ''
#     # Build statically linked binary
#     # This works best with languages like Go, Rust, C
#     # Lisp static linking is... challenging
#   '';
# });
# ```

# ### 2. **Docker/OCI Images** (Most practical)
# ```nix
# packages = forEachSupportedSystem ({ pkgs }: {
#   docker-image = pkgs.dockerTools.buildImage {
#     name = "my-app";
#     config = {
#       Cmd = [ "${packages.default}/bin/my-app" ];
#       Env = [ "LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [pkgs.portaudio pkgs.openssl]}" ];
#     };
#   };
# });
# ```

# ### 3. **AppImage** (Linux-specific portability)
# ```nix
# packages = forEachSupportedSystem ({ pkgs }: {
#   appimage = pkgs.appimageTools.wrapType2 {
#     name = "my-app";
#     src = packages.default;
#   };
# });
# ```

# ### 4. **Build for multiple targets**
# ```nix
# # Build separate binaries for each target
# packages = forEachSupportedSystem ({ pkgs }: {
#   default = /* your wrapped binary */;
# }) // {
#   # Cross-compile for other targets
#   "x86_64-linux-musl" = /* musl build */;
#   "aarch64-linux-static" = /* static ARM build */;
# };
# ```

# ## Recommendation

# For **maximum compatibility**, use **Docker images**. They're:
# - Portable across Linux distributions
# - Include all dependencies 
# - Easy to distribute
# - Work with container runtimes everywhere

#         The reality is: true "universal binaries" don't exist in the Linux ecosystem like they do for some other platforms. You need to pick your target platforms and build accordingly.

# Would it be possible to bake the LD_LIBRARY_PATH into the lisp image somehow? Though that seems very wierd.

# Yes, you can bake library paths into the Lisp image, and it's actually **not that weird** - it's a legitimate approach! Here are several ways to do it:

# ## Option 1: Set environment in the image at build time

# ```lisp
# ;; In your build script before saving the image
# (setf (uiop:getenv "LD_LIBRARY_PATH") 
#       "/nix/store/...-portaudio/lib:/nix/store/...-openssl/lib")

# ;; Then save the image
# #+sbcl (sb-ext:save-lisp-and-die "my-app" :executable t :toplevel #'main)
# ```

# ## Option 2: Modify library search paths programmatically

# ```lisp
# ;; Early in your application startup
# (cffi:*foreign-library-directories*
#   (append cffi:*foreign-library-directories*
#           '("/nix/store/.../portaudio/lib/"
#             "/nix/store/.../openssl/lib/")))

# ;; Or set CFFI search paths directly
# (push "/nix/store/.../portaudio/lib/" cffi:*foreign-library-directories*)
# ```

# ## Option 3: Use Nix to inject paths during build

# ```nix
# packages = forEachSupportedSystem ({ pkgs }: {
#   default = pkgs.runCommand "lisp-app" {
#     buildInputs = with pkgs; [ sbcl portaudio openssl ];
#   } ''
#     # Create a Lisp file with baked-in paths
#     cat > build.lisp << EOF
#     (setf (uiop:getenv "LD_LIBRARY_PATH") 
#           "${pkgs.lib.makeLibraryPath [ pkgs.portaudio pkgs.openssl ]}")
#     (load "your-app.lisp")
#     (sb-ext:save-lisp-and-die "$out/bin/my-app" :executable t :toplevel #'main)
#     EOF
    
#     sbcl --script build.lisp
#   '';
# });
# ```

# ## Option 4: CFFI-specific approach (cleanest)

# ```lisp
# ;; Define libraries with explicit paths at compile time
# (cffi:define-foreign-library portaudio
#   (:unix "/nix/store/.../portaudio/lib/libportaudio.so"))

# (cffi:define-foreign-library openssl  
#   (:unix "/nix/store/.../openssl/lib/libssl.so"))

# (cffi:use-foreign-library portaudio)
# (cffi:use-foreign-library openssl)
# ```

# ## Why this isn't weird:

# 1. **Embedded systems** do this all the time
# 2. **Application-specific library paths** are common
# 3. **Self-contained applications** often bundle their runtime environment
# 4. **Lisp images** are designed to capture complete runtime state

#         The **CFFI approach (#4)** is probably the cleanest since it's explicit about which libraries you're using and where they're located, making your application truly self-contained.

# User: But the first thing is sort of what I do. I only set the actual environment variable. But should this then not also be saved (if the above works). But it's not.

# You're absolutely right to be confused! The issue is that **environment variables are NOT saved in Lisp images** by default. When you do:

# ```lisp
# (setf (uiop:getenv "LD_LIBRARY_PATH") "/some/path")
# (sb-ext:save-lisp-and-die "my-app" :executable t)
# ```

# The `LD_LIBRARY_PATH` you set is only in the **current process** that's building the image. When the saved image starts up later, it gets a **fresh environment** from the shell that launches it.

# ## Why environment variables aren't saved:

# 1. **Security**: You don't want to bake in potentially sensitive env vars
# 2. **Flexibility**: The new process should inherit from its actual runtime environment  
# 3. **Design**: Lisp images save heap state, not process environment

# ## Solutions that actually work:

# ### Option 1: Set LD_LIBRARY_PATH in your main function
# ```lisp
# (defun main ()
#   ;; This runs every time the image starts
#   (setf (uiop:getenv "LD_LIBRARY_PATH") 
#         "/nix/store/...-portaudio/lib:/nix/store/...-openssl/lib")
#   ;; Now load your FFI libraries
#   (your-actual-main))
# ```

# ### Option 2: Use a startup hook (SBCL)
# ```lisp
# ;; Before saving the image
# (push (lambda () 
#         (setf (uiop:getenv "LD_LIBRARY_PATH") 
#               "/nix/store/.../lib:/nix/store/.../lib"))
#       sb-ext:*init-hooks*)

# (sb-ext:save-lisp-and-die "my-app" :executable t :toplevel #'main)
# ```

# ### Option 3: CFFI library paths (most reliable)
# ```lisp
# ;; This IS saved in the image
# (push "/nix/store/.../portaudio/lib/" cffi:*foreign-library-directories*)
# (push "/nix/store/.../openssl/lib/" cffi:*foreign-library-directories*)
# ```

# **Option 3 is probably what you want** - it directly tells CFFI where to find libraries, bypassing `LD_LIBRARY_PATH` entirely.
