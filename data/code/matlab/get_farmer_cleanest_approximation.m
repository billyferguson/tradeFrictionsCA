clear; 

[rho, p_e, H, SW, GW, gamma, L, aw, P, shares, ever_grown, noGW, alpha, N, N_d, K, T, unit_info] = load_data()

opt_nu = lsqnonlin(@(zz) get_sw_pi_cor(zz, L, shares, aw, SW, p_e, rho, H, gamma, GW, noGW, N_d, K, T), 1, 0)

pi = get_pi(opt_nu, shares, aw, p_e, rho, H, gamma, GW, noGW, N_d, K, T);

d = 1/opt_nu;


sw_mvs = zeros(N_d, T); 
max_SW = zeros(N_d, T); 
for t = 1:T
    sw_mvs(:, t) = p_e(:, t) .* rho .* (H(:, t) + gamma .* GW(:, t));
end
avg_mv = sum(sw_mvs, 1)/N_d;
weighted_avg_mv = sum(sw_mvs .* GW, 1)/sum(GW, 1);


opt_fallow_share = zeros(N, T);
for t = 1:T
    opt_fallow_share(:, t) = ones(N, 1)./(1 + sum(exp(d*pi(:, :, t)), 2));
end

opt_shares = zeros(N, K, T);
for t = 1:T
    opt_shares(:, :, t) = opt_fallow_share(:, t).*exp(d*pi(:, :, t)); 
end

opt_SW = squeeze(sum(L .* opt_shares .* aw, 2));

farmer_a = sw_mvs ./ (2 * (SW - opt_SW));
farmer_b = -2*farmer_a.*opt_SW; 

writematrix([unit_info(1:N_d, 1), farmer_a], "../../intermediate/farmer_approx_params/farmer_params_a" + t + ".csv");
writematrix([unit_info(1:N_d, 1), farmer_b], "../../intermediate/farmer_approx_params/farmer_params_b" + t + ".csv");


%%% NEED TO CHECK HERE IF THE APPROXIMATION LOOKS OK
%%% RIGHT NOW THE APPROXIMATION MATCHES TMODELS MV AT OPT SW AND OBS SW



%%%%%%%%%%%%%%% OLD 


% maximum SW is WRONG WRONG here cuz it maybe imply that they use more than
% optimal given a very large nu. Instead we need to find the optimal amount
% of SW. 
% for i = 1:N_d
%     for t = 1:T
%         max_aw_k = find(aw(i, :, t) == max(aw(i, :, t)));
%         max_SW(i, t) = max(L(i) * aw(i, max_aw_k(1), t), SW(i, t)); 
%     end
% end
% 
% 
% 
% 
% 
% 
% pi = get_pi(1, shares, aw, p_e, rho, H, gamma, GW, noGW, N_d, K, T);
% 
% % this is the relevant task here 
% approx_params = zeros(N_d, 3, T); 
% for i = 1:N_d
%     for t = 1:1
%         approx_params(i, 1:3, t) = get_approx_params(L(i), P(i, :, t),pi(i, :, t), aw(i, :, t), SW(i, t), max_SW(i, t), p_e(i, t), rho(i), H(i, t), gamma(i), noGW(i), 1, K, shares(i, :, t)', 0);
%     end
% end
% 
% % write approximated params to csv [to be cleaned in R]
% for t = 1:1
%     writematrix([unit_info(1:N_d, 1), approx_params(:, :, t)], "../../intermediate/farmer_approx_params/farmer_approx_params_" + t + ".csv");
% end
% 
% 
% %%%%%%%%%%%%%%% SCRATCH %%%%%%%%%%%%%%
% % compute profit per unit of water 
% profit_per_af_by_dauco = (squeeze(sum((square_pi ./ aw), 3)) ./ 5);
% avg_crop_shares_by_dauco = (squeeze(sum(shares, 3)) ./ 5) .* (1 - noGW) .* L;
% weighted_share_denom = squeeze(sum(avg_crop_shares_by_dauco, 1));
% avg_crop_profit = squeeze(sum(profit_per_af_by_dauco .* avg_crop_shares_by_dauco, 1)) ./ weighted_share_denom;
% writematrix(profit_per_af_by_dauco, "../../data/clean_data/dauco_profit_by_crop.csv"); 
% writematrix(avg_crop_shares_by_dauco, "../../data/clean_data/avg_crop_shares_by_dauco.csv"); 
% writematrix(avg_crop_profit, "../../data/clean_data/avg_profit_by_crop.csv"); 
% 
% 
% 
% 