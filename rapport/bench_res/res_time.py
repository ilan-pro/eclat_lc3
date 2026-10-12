import matplotlib.pyplot as plt

versions = ["Imperatif", "Fonctionnel", "Hybride", "OCaml", "VM-OCaml"]
cycles = [16.01, 22.49, 13.60, 0.04, 0.04]

plt.bar(versions, cycles)

plt.xlabel("Implémentation")
plt.ylabel("Temps d'exécution (s)")
plt.title("Comparaison des temps d'exécution")

plt.tight_layout()

plt.savefig("performance_time.png", dpi=300, bbox_inches="tight")

plt.show()