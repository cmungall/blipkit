CREATE TABLE adenine (
    b  VARCHAR
);

CREATE TABLE guanine (
    b  VARCHAR
);

CREATE TABLE uracil (
    b  VARCHAR
);

CREATE TABLE cytosine (
    b  VARCHAR
);

CREATE TABLE pairs_with_cWW (
    b  VARCHAR,
    target VARCHAR
);

CREATE TABLE five_prime_to (
    b  VARCHAR,
    target VARCHAR
);

CREATE VIEW pyrimidine AS
 SELECT 
   *
  FROM 
  cytosine
 UNION 
 SELECT 
  *
 FROM 
  guanine;

CREATE VIEW purine AS
 SELECT 
   *
  FROM 
  cytosine
 UNION 
 SELECT 
  *
 FROM 
  guanine;

CREATE VIEW gnra_tetraloop AS
SELECT DISTINCT 
  five_prime_to_1.b , 
  five_prime_to_5.target
 FROM 
  five_prime_to five_prime_to_1 , 
  five_prime_to five_prime_to_2 , 
  five_prime_to five_prime_to_3 , 
  five_prime_to five_prime_to_4 , 
  five_prime_to five_prime_to_5 , 
  pairs_with_cww pairs_with_cww_1 , 
  guanine guanine_1 , 
  purine purine_1 , 
  adenine adenine_1
 WHERE 
  five_prime_to_2.b = five_prime_to_1.target AND 
  five_prime_to_3.b = five_prime_to_2.target AND 
  five_prime_to_4.target = five_prime_to_4.b AND 
  five_prime_to_5.b = five_prime_to_4.b AND 
  pairs_with_cww_1.b = five_prime_to_1.b AND 
  pairs_with_cww_1.target = five_prime_to_5.target AND 
  guanine_1.b = five_prime_to_1.target AND 
  purine_1.b = five_prime_to_3.target AND 
  adenine_1.b = five_prime_to_4.b;

