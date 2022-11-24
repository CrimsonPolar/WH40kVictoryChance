#----------------------------------------------------------------------------------
# Helpful functions
#----------------------------------------------------------------------------------

# Function that calculates the chance of rolling >=n on 1d6
stat_to_prob <- function(n) {
  return((7-n)/6)
}

# Return probability to wound after hit
# Hits wound based on attacker strength vs target's toughness
# +------------------+
# |   Formula   | d6 |
# |-------------+----|
# |   s >= 2*t  | 2+ |
# |   s > t     | 3+ |
# |   s == t    | 4+ |
# |   s < t     | 5+ |
# |   s <= t/2  | 6+ |
# +------------------+
get_wound_prob <- function(s, t) {
  if (s >= 2*t) {
    return(stat_to_prob(2))
  }
  else if (s > t) {
    return(stat_to_prob(3))
  }
  else if (s == t) {
    return(stat_to_prob(4))
  }
  else if (s <= t/2) {
    return(stat_to_prob(5))
  }
  else if (s == t) {
    return(stat_to_prob(6))
  }
  else {
    # Shouldn't ever be here, but just for the sake of covering all possibilities
    return(0)
  }
}

#----------------------------------------------------------------------------------
# Start of script
#----------------------------------------------------------------------------------

# Target's wounds
target_w = 10

# Number of attacks the attacker has
attacks = 10
# Attacks hit on attacker_skill or higher on d6
attacker_skill = 3

# Stats to determine if hits wound
attacker_str = 6
target_tough = 4

# Damage characteristic of attacks
damage = 2

# Armor piercing subtracts from roll (increases roll required)
attack_ap = -2
# Save stats shows required die roll to resist an incoming wound, 1 always fails
target_sv = 3

# Feel no pain roll required to resist an unsaved wound, 0 means no feel no pain
# fnp = 5

#----------------------------------------------------------------------------------
# Calculate chance of target taking each wound, then apply cumulative binomial dist
# to calculate the chance of inflicting target_w wounds
#----------------------------------------------------------------------------------

# Probability to hit attack * probability to wound * probability to fail save
damage_prob = stat_to_prob(attacker_skill) * get_wound_prob(attacker_str, target_tough) * (1 - stat_to_prob(target_sv - attack_ap))

hits_required = ceiling(target_w / damage)

print("Probability of killing target unit with 1 attack sequence:")
print(1 - pbinom(hits_required - 1, attacks, damage_prob))