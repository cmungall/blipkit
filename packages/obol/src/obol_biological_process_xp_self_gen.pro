process5(P that regulates(RegulatedProcess)) --> regulation(P),[of],process(RegulatedProcess).
process5(P that negatively_regulates(RegulatedProcess)) --> negative_regulation(P),[of],process(RegulatedProcess).
process5(P that positively_regulates(RegulatedProcess)) --> positive_regulation(P),[of],process(RegulatedProcess).

process5(P that regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        regulation(P),[of],process(ModulatedProcess),[in],biological_role(ModulateeRole).
process5(P that negatively_regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        negative_regulation(P),[of],process(ModulatedProcess),[in],biological_role(ModulateeRole).
process5(P that positively_regulates(ModulatedProcess) and regulates_process_in(ModulateeRole)) -->
        positive_regulation(P),[of],process(ModulatedProcess),[in],biological_role(ModulateeRole).
