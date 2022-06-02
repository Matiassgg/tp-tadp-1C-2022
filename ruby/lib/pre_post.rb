class PrePost
  def initialize(context, params, args, block, result = nil)
    params.zip(args).each do |param, arg|
      context.define_singleton_method(param) do
        arg
      end
    end
    @block = block
    @context = context
    @result = result
  end

  def exec
    if @result.nil?
      @context.instance_eval(&@block)
    else
      @context.instance_exec(@result, &@block)
    end
  end
end
