# frozen_string_literal: true

class Object
  def not_nil?
    !nil?
  end

  alias present? not_nil?
end
