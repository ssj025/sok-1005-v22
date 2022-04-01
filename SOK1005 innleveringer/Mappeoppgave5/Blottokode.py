import blotto

import numpy as np
#veldig lik den på computer strategy men fant ingen andremåter å gjøre det på
def player_strategy(n_battalions,n_fields):
    #defining the array:
    battalions=np.zeros(n_fields,dtype=int)
    battalions[0]=34
    battalions[1]=24
    battalions[2]=36
    battalions[3:]=2
    




    #asserting that all and no more than all battalions are used:  sum=100
    battalions=battalions[np.random.rand(n_fields).argsort()]
    assert sum(battalions)==n_battalions
    return battalions

def computer_strategy(n_battalions,n_fields):
    battalions=np.zeros(n_fields,dtype=int)
    battalions[0:1]=8
    battalions[1:4]=30
    battalions[4:]=1
    assert sum(battalions)==n_battalions
    return battalions    



def call_battle(n_battalions,n_fields, player_strategy, computer_strategy):
    c_battlions=computer_strategy(n_battalions,n_fields)
    p_battlions=player_strategy(n_battalions,n_fields)

    diff=p_battlions-c_battlions
    points=sum(diff>0)-sum(diff<0)
 
    return int(points>0)-int(points<0)


def test_strategies(n_fields,n_battalions,player_strategy, computer_strategy):
    n_tests=100000
    r=0
    record=[]
    for i in range(n_tests):
        p=call_battle(n_battalions,n_fields,
            player_strategy, computer_strategy)
        record.append(p)
        r+=p
    return r/n_tests
#blotto.run(6, 100, player_strategy, computer_strategy)
r=test_strategies(6,100,player_strategy, computer_strategy)
print(r)