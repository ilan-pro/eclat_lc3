import matplotlib.pyplot as plt

versions = ["Fonctionnel", "Impératif", "Hybride", "VM-OCaml"]
cycles = [500, 5225, 1000, 774]

plt.bar(versions, cycles)

plt.xlabel("Version du processeur")
plt.ylabel("Nombre de cycles")
plt.title("Comparaison du nombre de cycles")

plt.tight_layout()
plt.savefig("performance_cycles.png", dpi=300, bbox_inches="tight")
plt.show()