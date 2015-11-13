module Rebar
  def self.fetch_repos_for(node, os_token)
    res = {}
    (node[:rebar][:package_repos] || [] rescue []).each do |repo|
      urls = repo['oses'].select{|os|os_token.start_with?(os['os'])}.flat_map{|os|os['repos']}.sort.uniq
      next if urls.empty?
      res[repo['name']] = urls
    end
    Chef::Log.info(res.inspect)
    res
  end
end
