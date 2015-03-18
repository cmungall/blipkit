% standard is_a propagation
some(G,R,X) <- some(G,R,X2),subclass(X2,X).

% transitive part_of
some(G,part_of,C) <- some(G,part_of,C2),parent(C2,part_of,C).

some(G,is_active_participant_in,P) <- some(G,is_active_participant_in,P2),parent(P2,part_of,P).
some(G,is_active_participant_in,P) <- some(G,executes,P2),parent(P2,part_of,P).

% regulates
some(G,regulates,P) <- some(G,executes,P2),parent(P2,regulates,P).
some(G,negatively_regulates,P) <- some(G,executes,P2),parent(P2,negatively_regulates,P).
some(G,positively_regulates,P) <- some(G,executes,P2),parent(P2,positively_regulates,P).

some(G,regulates,P) <- some(G,is_active_participant_in,P2),parent(P2,regulates,P).
some(G,negatively_regulates,P) <- some(G,is_active_participant_in,P2),parent(P2,negatively_regulates,P).
some(G,positively_regulates,P) <- some(G,is_active_participant_in,P2),parent(P2,positively_regulates,P).

some(G,regulates,P) <- some(G,regulates,P2),parent(P2,part_of,P).

some(G,indirectly_regulates,P) <- some(G,regulates,P2),parent(P2,regulates,P).
some(G,indirectly_negatively_regulates,P) <- some(G,negatively_regulates,P2),parent(P2,positively_regulates,P).
some(G,indirectly_positively_regulates,P) <- some(G,positively_regulates,P2),parent(P2,positively_regulates,P).
some(G,indirectly_positively_regulates,P) <- some(G,positively_regulates,P2),parent(P2,positively_regulates,P).

% INTEGRAL TO

integral_to(G,part_of,C_Big) <- integral_to(G,part_of,C_Small),parent(C_Big,has_part,C_Small).
integral_to(G,is_active_participant_in,C_Big) <- integral_to(G,is_active_participant_in,C_Small),parent(C_Big,has_part,C_Small).

% reverse propagation
integral_to(G,R,C_Specific) <- integral_to(G,R,C_Generic),subclass(C_Specific,C_Generic).

