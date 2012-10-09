import argparse
import random

def polya_urn_model(base_distribution, num_balls, alpha):
    if not num_balls: return []

    balls_in_urn = []
    for i in xrange(num_balls):
        if random.random() < float(alpha) / (len(balls_in_urn) + alpha):
            balls_in_urn.append( base_distribution() )
        else:
            ball = random.choice(balls_in_urn)
            balls_in_urn.append(ball)

    return ['{0:.3f}'.format(b) for b in balls_in_urn]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-n', help='number of balls to sample')
    parser.add_argument('-a', help='alpha parameter')
    args = parser.parse_args()

    unif_dist = random.random

    urn_contents = polya_urn_model(unif_dist, int(args.n), float(args.a))
    print urn_contents


if (__name__ == '__main__'):
    main()
