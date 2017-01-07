class ChatsController < ApplicationController

  before_action :authenticate_user!

  def index
    @username = current_user.email
  end
end
