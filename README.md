# HegelPlus
A qualified-tree automata based approach for component-based synthesis, accompanying as a supplemental material 
for the main paper : Deductive Component-Based Synthesis Using Semantic Similarity Reduction


# Dependencies:
1. OCaml (Version >= 4.03)
2. Z3 
3. OCamlLex 

# To build:
cd prundent-home
./build.sh 

# To run Hegel
<!-- The default run is Hegel -->
./prudent.native  -max <maximum-path-length> <path-to-spec-file>
<!-- To run ablation studies -->
./prudent.native [-nosimilarity] [-nolca] [-static] -max <maximum-path-length> <path-to-spec-file>

# Example
./prudent.native synth_tests/unit/checked/other_units/u_test3.spec 


# Main Directory structure

./benhmakrs/Hoogle+       Hoogle+ and ECTA benchmarks in the paper
./benchmarks/fromCobalt   The longer database benchmarks
./main/prudent.ml         Main Hegel 
./synthesis               Synthesis algorithm and related
./synlang                 The synthesis language \lambda_{qta}
./typing                  The underlying refinement type system# HegelPlus
