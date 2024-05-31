\ Model amplgurobi
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  C1 + C2 - C5 + C7
Subject To
 R0: - C1 - 4 C5 >= -12
 R1: 3 C0 - C1 >= -1
 R2: 6.5 C0 - C3 >= 3
 R3: 6 C0 + 0.5 C1 - C4 >= 3
 R4: 0.5 C0 + 0.5 C2 - C5 >= 0.25
 R5: 6 C1 - C4 >= 0
 R6: 6 C1 - C6 >= 0
 R7: 0.5 C1 - C7 >= 0
 R8: 6 C2 - C5 >= 0
 R9: 6 C2 - C7 >= 0
 R10: 0.5 C2 - C8 >= 0
 R11: - C0 + C3 >= -0.25
 R12: - 0.5 C1 + C4 >= 0
 R13: - 0.5 C2 + C5 >= 0
 R14: C6 >= 0
 R15: C7 >= 0
 R16: C8 >= 0
 R17: - 12 C0 + C3 >= -36
 R18: - 6 C0 - 6 C1 + C4 >= -36
 R19: - 0.5 C0 - 6 C2 + C5 >= -3
 R20: - 12 C1 + C6 >= -36
 R21: - 0.5 C1 - 6 C2 + C7 >= -3
 R22: - C2 + C8 >= -0.25
Bounds
 0.5 <= C0 <= 6
 C1 <= 6
 C2 <= 0.5
End
