from typing import List, Tuple, Generator
import numpy as np
import random


class UserParameterEnum:
    INEQUALITY_AVERSION = [0, 0.4, 1]
    RISK_AVERSION = [0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8]
    THEORY_OF_MIND_SOPHISTICATION = [0, 1, 2, 3, 4]
    PLANNING = [1, 2, 3, 4]
    IRRITABILITY = [0, 0.25, 0.5, 0.75, 1]
    IRRITATION_AWARENESS = [0, 1, 2, 3, 4]
    INVERSE_TEMPERATURE = [1/4, 1/3, 1/2, 1]


class UserParameter:
    inequalityAversion: float  # {0, 0.4, 1}
    riskAversion: float  # {0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8}
    theoryOfMindSophistication: int  # {0, 1, 2, 3, 4}
    planning: float  # {1, 2, 3, 4}
    irritability: float  # {0, 0.25, 0.5, 0.75, 1}
    irritationAwareness: int  # {0, 1, 2, 3, 4}
    inverseTemperature: float  # {1/4, 1/3, 1/2, 1/1}

    def __init__(self, inequalityAversion: float, riskAversion: float, theoryOfMindSophistication: int, planning: float,
                 irritability: float, irritationAwareness: int, inverseTemperature: float):
        self.inequalityAversion = inequalityAversion
        self.riskAversion = riskAversion
        self.theoryOfMindSophistication = theoryOfMindSophistication
        self.planning = planning
        self.irritability = irritability
        self.irritationAwareness = irritationAwareness
        self.inverseTemperature = inverseTemperature

def gen_user_para() -> Generator[UserParameter]:
    for inequalityAversion in UserParameterEnum.INEQUALITY_AVERSION:
        for riskAversion in UserParameterEnum.RISK_AVERSION:
            for theoryOfMindSophistication in UserParameterEnum.THEORY_OF_MIND_SOPHISTICATION:
                for planning in UserParameterEnum.PLANNING:
                    for irritability in UserParameterEnum.IRRITABILITY:
                        for irritationAwareness in UserParameterEnum.IRRITATION_AWARENESS:
                            for inverseTemperature in UserParameterEnum.INVERSE_TEMPERATURE:
                                yield UserParameter(
                                    inequalityAversion,
                                    riskAversion,
                                    theoryOfMindSophistication,
                                    planning,
                                    irritability,
                                    irritationAwareness,
                                    inverseTemperature
                                )


class State:
    round: int  # round = len(history) + 1
    history: List[Tuple[int, int]]  # [(investor's action), (trustee's action)]
    # irritation: float  # irritation level of the investor
    # ... other state variables

    def __init__(self, round: int, history: List[Tuple[int, int]]):
        self.round = round
        self.history = history


def policy(user_parameter: UserParameter, state: State) -> List[float]:
    """
    Determines the investor's policy as a probability distribution over possible actions.

    Args:
        user_parameter (UserParameter): The individual parameters that may influence the policy.
        state (State): The current state of the game&individual that which may include various factors affecting the decision.

    Returns:
        List[float]: A list representing the probability distribution over possible actions:
                     [p(invest 0), p(invest 5), p(invest 10), p(invest 15), p(invest 20)].
    """
    # The policy function can be very complex, 
    # with some parts of the code being predetermined based on prior knowledge,
    # while other free parts can be delegated to large language models.
    pass


class ExperimentData:
    trustGameData: List[Tuple[int, int]]

data: List[ExperimentData]
# TODO: load data from the database

def evaluate_BIC():
    # Return the Bayesian Information Criterion (BIC) of the policy
    BIC_score = 0
    for i in range(len(data)):
        nll = 1e9   # negative loglikelihood
        best_para: UserParameter = None
        for para in gen_user_para():
            s = 0
            for j in range(len(data[i].trustGameData) - 1):
                state = State(j, data[i].trustGameData[:j])
                action_prob = policy(para, state)
                s += -np.log(action_prob[data[i].trustGameData[j][0] / 5])
            if s < nll:
                nll = s
                best_para = para
        BIC_score += nll + 7/2 * (np.log(len(data[i].trustGameData)) - np.log(2 * np.pi))
    return BIC_score
