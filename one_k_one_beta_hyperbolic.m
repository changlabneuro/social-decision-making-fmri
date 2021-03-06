
function [f] = one_k_one_beta_hyperbolic(p,chosen,delay_left,delay_right,reward_left,reward_right,agent,outtype)


%%%%% 1. Assign free parameters:
base = 1;
discount = p(1);

beta = p(2);

all_prob = [];
self_prob = [];
other_prob = [];

V_self  =  [];
V_other =  [];
V_all   =  [];


%%%% Model -hyperbolically devalue reward by effort. 1 parameter for both
%%%% conditions


val = ( reward_right ./ (1 + (discount.*(delay_right))) ) - ( reward_left ./ (1 + (discount.*(delay_left))) );

prob =  exp(val.*beta)./(exp(base*beta) + exp(beta.*val));
prob(~chosen) =  1 - prob(~chosen);
prob = prob(:,1);

% calculate neg-log-likelihood
f=-nansum(log(prob));



end